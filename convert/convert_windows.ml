(* virt-v2v
 * Copyright (C) 2009-2025 Red Hat Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *)

open Printf

open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Utils
open Types

module G = Guestfs

(* Convert Windows guests.
 *
 * This only does a "pre-conversion", the steps needed to get the
 * Windows guest to boot on KVM.  Unlike the [Convert_linux] module,
 * this is not a full conversion.  Instead it just installs the
 * [viostor] (Windows virtio block) driver, so that the Windows guest
 * will be able to boot on the target.  A [RunOnce] script is also
 * added to the VM which does all the rest of the conversion the first
 * time the Windows VM is booted on KVM.
 *)

let convert (g : G.guestfs) source inspect i_firmware
      block_driver _ static_ips =
  (*----------------------------------------------------------------------*)
  (* Inspect the Windows guest. *)

  (* Locate virtio-win ISO or directory using the [VIRTIO_WIN]
   * environment variable.
   *)
  let virtio_win =
    Inject_virtio_win.from_environment g inspect.i_root Config.datadir in
  (match block_driver with
   | Virtio_blk -> () (* the default, no need to do anything *)
   | Virtio_SCSI ->
      let drivers = Inject_virtio_win.get_block_driver_priority virtio_win in
      let drivers = "vioscsi" :: drivers in
      Inject_virtio_win.set_block_driver_priority virtio_win drivers
   | IDE -> assert false (* not possible - but maybe ...? *)
  );

  (* If the Windows guest appears to be using group policy.
   *
   * Since this was written, it has been noted that it may be possible
   * to remove this restriction:
   *
   * 12:35 < StenaviN> here is the article from MS: https://support.microsoft.com/uk-ua/help/2773300/stop-0x0000007b-error-after-you-use-a-group-policy-setting-to-prevent
   * 12:35 < StenaviN> aside of that the following registry hive should be deleted as well: [-HKEY_LOCAL_MACHINE\SOFTWARE\Policies\Microsoft\Windows\DeviceInstall]
   * 12:36 < StenaviN> more precisely, [-HKEY_LOCAL_MACHINE\SOFTWARE\Policies\Microsoft\Windows\DeviceInstall\Restrictions]
   *)
  let has_group_policy =
    Registry.with_hive_readonly g inspect.i_windows_software_hive
      (fun reg ->
       try
         let path = ["Microsoft"; "Windows"; "CurrentVersion";
                     "Group Policy"; "History"]  in
         let node =
           match Registry.get_node reg path with
           | None -> raise Not_found
           | Some node -> node in
         let children = g#hivex_node_children node in
         let children = Array.to_list children in
         let children =
           List.map (fun { G.hivex_node_h = h } -> g#hivex_node_name h)
                    children in
         (* Just assume any children looking like "{<GUID>}" mean that
          * some GPOs were installed.
          *
          * In future we might want to look for nodes which match:
          * History\{<GUID>}\<N> where <N> is a small integer (the order
          * in which policy objects were applied.
          *
          * For an example registry containing GPOs, see RHBZ#1219651.
          * See also: https://support.microsoft.com/en-us/kb/201453
          *)
         let is_gpo_guid name =
           let len = String.length name in
           len > 3 && name.[0] = '{' &&
             Char.isxdigit name.[1] && name.[len-1] = '}'
         in
         List.exists is_gpo_guid children
       with
         Not_found -> false
      ) in

  (* If the Windows guest has AV installed. *)
  let has_antivirus = Windows.detect_antivirus inspect in

  (* Does the guest expect the RTC to be set to UTC or localtime?
   * See https://wiki.archlinux.org/title/System_time#UTC_in_Microsoft_Windows
   * Note this might be a QWORD on 64 bit Windows instances.
   *)
  let rtc_utc =
    Registry.with_hive_readonly g inspect.i_windows_system_hive
      (fun reg ->
       try
         let key_path = [ "Control"; "TimeZoneInformation" ] in
         let path = inspect.i_windows_current_control_set :: key_path in
         let node =
           match Registry.get_node reg path with
           | None -> raise Not_found
           | Some node -> node in
         let valueh = g#hivex_node_get_value node "RealTimeIsUniversal" in
         if valueh = 0L then raise Not_found;
         let t = g#hivex_value_type valueh in
         let data = g#hivex_value_value valueh in
         let is_utc =
           match t with
           | 0_L (* REG_NONE *) -> false (* localtime *)
           | 4_L (* REG_DWORD *) -> data = "\001\000\000\000"
           | 11_L (* REG_QWORD *) -> data = "\001\000\000\000\000\000\000\000"
           | _ (* who knows ... *) ->
             warning (f_"unknown CurrentControlSet\\Control\\\
                         TimeZoneInformation key RealTimeIsUniversal \
                         type 0x%Lx, assuming RTC set to UTC") t;
             true in
         is_utc
       with Not_found ->
         (* If the key is not found then by default we assume
          * that Windows is expecting the RTC to be set to localtime.
          *)
         false
      ) in

  (* Open the software hive (readonly) and find the Xen PV uninstaller,
   * if it exists.
   *)
  let xenpv_uninst =
    let xenpvreg = "Red Hat Paravirtualized Xen Drivers for Windows(R)" in

    Registry.with_hive_readonly g inspect.i_windows_software_hive
      (fun reg ->
       try
         let path = ["Microsoft"; "Windows"; "CurrentVersion"; "Uninstall";
                     xenpvreg] in
         let node =
           match Registry.get_node reg path with
           | None -> raise Not_found
           | Some node -> node in
         let uninstkey = "UninstallString" in
         let valueh = g#hivex_node_get_value node uninstkey in
         if valueh = 0L then (
           warning (f_"cannot uninstall Xen PV drivers: registry key ‘HKLM\\\
                       SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\\
                       Uninstall\\%s’ does not contain an ‘%s’ key")
             xenpvreg uninstkey;
           raise Not_found
         );
         let data = g#hivex_value_value valueh in
         let data = Registry.decode_utf16le data in

         (* The uninstall program will be uninst.exe.  This is a wrapper
          * around _uninst.exe which prompts the user.  As we don't want
          * the user to be prompted, we run _uninst.exe explicitly.
          *)
         let len = String.length data in
         let data =
           if len >= 8 &&
              String.lowercase_ascii (String.sub data (len-8) 8) = "uninst.exe"
           then
             (String.sub data 0 (len-8)) ^ "_uninst.exe"
           else
             data in

         Some data
       with
         Not_found -> None
      ) in

  (* Locate and retrieve all the uninstallation commands for installed
   * applications.
   *)
  let uninstallation_commands pretty_name matchfn modfn extra_uninstall_params =
    let path = ["Microsoft"; "Windows"; "CurrentVersion"; "Uninstall"] in
    let uninstval = "UninstallString" in
    let ret = ref [] in

    Registry.with_hive_readonly g inspect.i_windows_software_hive (
      fun reg ->
        match Registry.get_node reg path with
        | None -> ()
        | Some node ->
           let uninstnodes = g#hivex_node_children node in
           Array.iter (
             fun { G.hivex_node_h = uninstnode } ->
               let valueh = g#hivex_node_get_value uninstnode "DisplayName" in
               if valueh <> 0L then (
                 let dispname = g#hivex_value_string valueh in
                 if matchfn dispname then (
                   let valueh = g#hivex_node_get_value uninstnode uninstval in
                   if valueh <> 0L then (
                     let reg_cmd = g#hivex_value_string valueh in
                     let reg_cmd = modfn reg_cmd in
                     let cmd =
                       sprintf "%s /quiet /norestart /l*v+ \"%%~dpn0.log\" \
                                REBOOT=ReallySuppress REMOVE=ALL %s"
                         reg_cmd extra_uninstall_params in
                     List.push_front cmd ret
                   )
                   else
                     let name = g#hivex_node_name uninstnode in
                     warning (f_"cannot uninstall %s: registry key \
                                 ‘HKLM\\SOFTWARE\\%s\\%s’ with DisplayName \
                                 ‘%s’ doesn't contain value ‘%s’")
                       pretty_name (String.concat "\\" path) name
                       dispname uninstval
                 )
               )
             ) uninstnodes
    ) (* with_hive_readonly *);
    !ret
  in

  (* Locate and retrieve all uninstallation commands for Parallels Tools. *)
  let prltools_uninsts =
    let matchfn s =
      String.find s "Parallels Tools" != -1 ||
      String.find s "Virtuozzo Tools" != -1
    in
    (* Without these custom Parallels-specific MSI properties the
     * uninstaller still shows a no-way-out reboot dialog.
     *)
    let extra_uninstall_params =
      "PREVENT_REBOOT=Yes LAUNCHED_BY_SETUP_EXE=Yes" in
    uninstallation_commands "Parallels Tools" matchfn Fun.id
      extra_uninstall_params in

  (* Locate and retrieve all uninstallation commands for VMware Tools. *)
  let vmwaretools_uninst =
    let matchfn s =
      String.find s "VMware Tools" != -1
    in
    (* VMware Tools writes the install command (MsiExec /I) into the
     * UninstallString key in the registry, rather than the uninstall
     * command.  Try to spot this and rewrite.  (RHBZ#1917760).
     *)
    let re1 = PCRE.compile ~caseless:true "msiexec" in
    let re2 = PCRE.compile ~caseless:true "/i" in
    let msifn s = if PCRE.matches re1 s then PCRE.replace re2 "/x" s else s in
    uninstallation_commands "VMware Tools" matchfn msifn "" in

  (*----------------------------------------------------------------------*)
  (* Perform the conversion of the Windows guest. *)

  let rec do_convert () =
    (* Firstboot configuration. *)
    configure_firstboot ();

    (* Open the system hive for writes and update it. *)
    let { Inject_virtio_win.block_driver; net_driver} as virtio_win_installed =
      Registry.with_hive_write g inspect.i_windows_system_hive
                               update_system_hive in

    (* Open the software hive for writes and update it. *)
    Registry.with_hive_write g inspect.i_windows_software_hive
                             update_software_hive;

    configure_online_disks block_driver;

    configure_network_interfaces net_driver;

    fix_ntfs_heads ();

    fix_win_esp ();

    (* Warn if installation of virtio block drivers might conflict with
     * group policy or AV software causing a boot 0x7B error (RHBZ#1260689).
     *)
    if block_driver = Virtio_blk then (
      if has_group_policy then
        warning (f_"this guest has Windows Group Policy Objects (GPO) and a \
                    new virtio block device driver was installed.  In some \
                    circumstances, Group Policy may prevent new drivers from \
                    working (resulting in a 7B boot error).  If this happens, \
                    try disabling Group Policy before doing the conversion.");
      if has_antivirus then
        warning (f_"this guest has Anti-Virus (AV) software and a new virtio \
                    block device driver was installed.  In some \
                    circumstances, AV may prevent new drivers from working \
                    (resulting in a 7B boot error).  If this happens, try \
                    disabling AV before doing the conversion.");
    );

    (* Return guest capabilities from the convert () function. *)
    let guestcaps = {
      gcaps_block_bus = of_virtio_win_block_type block_driver;
      gcaps_net_bus = of_virtio_win_net_type net_driver;
      gcaps_virtio_rng = virtio_win_installed.Inject_virtio_win.virtio_rng;
      gcaps_virtio_balloon =
        virtio_win_installed.Inject_virtio_win.virtio_balloon;
      gcaps_isa_pvpanic = virtio_win_installed.Inject_virtio_win.isa_pvpanic;
      gcaps_virtio_socket =
        virtio_win_installed.Inject_virtio_win.virtio_socket;
      gcaps_machine = of_virtio_win_machine_type
                        virtio_win_installed.Inject_virtio_win.machine;
      gcaps_arch = Utils.kvm_arch inspect.i_arch;
      gcaps_arch_min_version = 0;
      gcaps_virtio_1_0 = virtio_win_installed.Inject_virtio_win.virtio_1_0;
      gcaps_rtc_utc = rtc_utc;
    } in

    guestcaps

  and of_virtio_win_block_type = function
    | Inject_virtio_win.Virtio_blk -> Virtio_blk
    | Virtio_SCSI -> Virtio_SCSI
    | IDE -> IDE

  and of_virtio_win_net_type = function
    | Inject_virtio_win.Virtio_net -> Virtio_net
    | E1000 -> E1000
    | RTL8139 -> RTL8139

  and of_virtio_win_machine_type = function
    | Inject_virtio_win.I440FX -> I440FX
    | Q35 -> Q35
    | Virt -> Virt

  and configure_firstboot () =
    (* Run the firstboot script with pnputil.exe before the one with
     * pnp_wait.exe as the latter suppresses PnP for all following scripts.
     *)
    configure_pnputil_install ();

    let tool_path = virt_tools_data_dir () // "pnp_wait.exe" in
    if Sys.file_exists tool_path then
      configure_wait_pnp tool_path
    else
      debug (f_"%s is missing.  Firstboot scripts may conflict with PnP.")
        tool_path;

    (* Install VMDP unconditionally, if available, but don't
     * warn about it if not.
     *)
    let tool_path = virt_tools_data_dir () // "vmdp.exe" in
    if Sys.file_exists tool_path then
      configure_vmdp tool_path;

    (* Install QEMU Guest Agent unconditionally and warn if missing *)
    if not (Inject_virtio_win.inject_qemu_ga virtio_win) then
      warning (f_"QEMU Guest Agent MSI not found on tools ISO/directory. You \
                  may want to install the guest agent manually after \
                  conversion.");

    (* Install Balloon Server unconditionally and warn if missing *)
    if not (Inject_virtio_win.inject_blnsvr virtio_win) then
      warning (f_"Balloon Server (blnsvr.exe) not found on tools \
                  ISO/directory. You may want to install this component \
                  manually after conversion.");

    unconfigure_xenpv ();
    unconfigure_prltools ();
    unconfigure_vmwaretools ()

  (* [set_reg_val_dword_1 path name] creates a registry key
   * called [name = dword:1] in the registry [path].
   * Intermediate nodes are created along the path if required.
   *
   * It returns the old value, if there was one, else [None].
   *)
  and set_reg_val_dword_1 ((g, root) as reg) path name =
    let node = Registry.create_path reg path in
    let valueh = g#hivex_node_get_value node name in
    let value =
      match valueh with
      | 0L -> None
      | _ -> Some (int_of_le32 (g#hivex_value_value valueh)) in
    g#hivex_node_set_value node name 4_L (le32_of_int 1_L);
    value

  and reg_restore key name value =
    let strkey = String.concat "\\" key in
    match value with
    | Some value -> sprintf "reg add \"%s\" /v %s /t REG_DWORD /d %Ld /f"
                      strkey name value
    | None -> sprintf "reg delete \"%s\" /v %s /f" strkey name

  and configure_pnputil_install () =
    let fb_script = {|@echo off

setlocal EnableDelayedExpansion
set inf_dir=%systemroot%\Drivers\Virtio\
echo Installing drivers from %inf_dir%
set REBOOT_PENDING=0

reg query "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\WindowsUpdate\Auto Update\RebootRequired"
if %errorlevel%==0 (
echo Windows Update: Reboot required.
set REBOOT_PENDING=1
)

reg query "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Component Based Servicing\RebootPending"
if %errorlevel%==0 (
echo CBS: Reboot required.
set REBOOT_PENDING=1
)

reg query "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager" /v PendingFileRenameOperations
if %errorlevel%==0 (
echo Session Manager: Reboot required.
set REBOOT_PENDING=1
)

if "%REBOOT_PENDING%"=="1" (
echo A reboot is pending.
exit /b 249
) else (
echo No pending reboot detected.
)

for %%f in ("%inf_dir%*.inf") do (
echo Installing: %%~nxf.
%systemroot%\Sysnative\PnPutil -i -a "%%f"
if !errorlevel! neq 0 if !errorlevel! neq 259 (
echo Failed to install %%~nxf.
exit /b 249
) else (
echo Successfully installed %%~nxf.
)
)
echo All drivers installed successfully.
exit /b 0
)|} in

    (* Set priority higher than that of "network-configure" firstboot script. *)
    Firstboot.add_firstboot_script g inspect.i_root ~prio:2000
      "pnputil install drivers" fb_script;

  and configure_wait_pnp tool_path =
    (* Prevent destructive interactions of firstboot with PnP. *)

    (* Suppress "New Hardware Wizard" until PnP settles (see
     * https://support.microsoft.com/en-us/kb/938596) and restore it
     * afterwards.
     *)
    let reg_restore_str =
      match inspect.i_major_version, inspect.i_minor_version with
      (* WinXP 32bit *)
      | 5, 1 ->
         let key_path = ["Policies"; "Microsoft"; "Windows"; "DeviceInstall";
                         "Settings"] in
         let name = "SuppressNewHWUI" in
         let value =
           Registry.with_hive_write g inspect.i_windows_software_hive (
             fun reg -> set_reg_val_dword_1 reg key_path name
         ) in
         reg_restore ("HKLM\\Software" :: key_path) name value

      (* WinXP 64bit / Win2k3 *)
      | 5, 2 ->
         let key_path = ["Services"; "PlugPlay"; "Parameters"] in
         let name = "SuppressUI" in
         let value =
           Registry.with_hive_write g inspect.i_windows_system_hive (
             fun reg ->
               let path = inspect.i_windows_current_control_set :: key_path in
               set_reg_val_dword_1 reg path name
           ) in
         reg_restore ("HKLM\\SYSTEM\\CurrentControlSet" :: key_path) name
                     value

      (* any later Windows *)
      | _ -> "" in

    let pnp_wait_path = "/Program Files/Guestfs/Firstboot/pnp_wait.exe" in

    let fb_script = sprintf
                      {|@echo off

echo Wait for PnP to complete
"%s"
%s|}
                      (String.replace_char pnp_wait_path '/' '\\')
                      reg_restore_str in

    Firstboot.add_firstboot_script g inspect.i_root "wait pnp" fb_script;
    (* add_firstboot_script has created the path already. *)
    g#upload tool_path (g#case_sensitive_path pnp_wait_path)

  and configure_vmdp tool_path =
    (* Configure VMDP if possible *)
    g#upload tool_path "/vmdp.exe";

    let fb_script = {|echo V2V first boot script started
echo Decompressing VMDP installer
"\vmdp.exe"
pushd "VMDP-*"
echo Installing VMDP
setup.exe /eula_accepted /no_reboot
popd
|} in

    let fb_recover_script = {|echo Finishing VMDP installation
if not exist VMDP-* (
"\vmdp.exe"
)
pushd "VMDP-*"
setup.exe /eula_accepted /no_reboot
popd
|} in

    Firstboot.add_firstboot_script g inspect.i_root
      "configure vmdp" fb_script;

    Firstboot.add_firstboot_script g inspect.i_root
      "finish vmdp setup" fb_recover_script

  and unconfigure_xenpv () =
    match xenpv_uninst with
    | None -> () (* nothing to be uninstalled *)
    | Some uninst ->
      let fb_script = sprintf
                        {|@echo off

echo uninstalling Xen PV driver
"%s"
|}
                        uninst in
      Firstboot.add_firstboot_script g inspect.i_root
        "uninstall Xen PV" fb_script

  and unconfigure_prltools () =
    let regkey =
      "HKLM\\System\\CurrentControlSet\\Services\\prl_strg\\DriverInfo" in
    List.iter (
      fun uninst ->
        let fb_script = sprintf
                          {|@echo off

REG DELETE %s /v RefCount /f

echo uninstalling Parallels guest tools
rem ERROR_SUCCESS_REBOOT_REQUIRED (3010) is OK too
%s
if errorlevel 3010 exit /b 0
|}
                          regkey uninst in

        Firstboot.add_firstboot_script g inspect.i_root
          "uninstall Parallels tools" fb_script
    ) prltools_uninsts

  and unconfigure_vmwaretools () =
    List.iter (
      fun uninst ->
        let fb_script = sprintf
                          {|@echo off

echo uninstalling VMware Tools
rem ERROR_SUCCESS_REBOOT_REQUIRED (3010) is OK too
%s
if errorlevel 3010 exit /b 0
|}
                          uninst in

        Firstboot.add_firstboot_script g inspect.i_root
          "uninstall VMware Tools" fb_script
    ) vmwaretools_uninst

  and update_system_hive reg =
    (* Update the SYSTEM hive.  When this function is called the hive has
     * already been opened as a hivex handle inside guestfs.
     *)
    disable_xenpv_win_drivers reg;
    disable_prl_drivers reg;
    disable_autoreboot reg;
    Inject_virtio_win.inject_virtio_win_drivers virtio_win reg

  and disable_xenpv_win_drivers reg =
    (* Disable xenpv-win service (RHBZ#809273). *)
    let services =
      Registry.get_node reg
                        [inspect.i_windows_current_control_set; "Services"] in

    match services with
    | None -> ()
    | Some services ->
       let node = g#hivex_node_get_child services "rhelscsi" in
       if node <> 0L then
         g#hivex_node_set_value node "Start" 4_L (le32_of_int 4_L)

  and disable_prl_drivers reg =
    (* Prevent Parallels drivers from loading at boot. *)
    let services =
      Registry.get_node reg
                        [inspect.i_windows_current_control_set; "Services"] in
    let prl_svcs = [ "prl_boot"; "prl_dd"; "prl_eth5"; "prl_fs"; "prl_memdev";
                     "prl_mouf"; "prl_pv32"; "prl_pv64"; "prl_scsi";
                     "prl_sound"; "prl_strg"; "prl_tg"; "prl_time";
                     "prl_uprof"; "prl_va" ] in

    match services with
    | None -> ()
    | Some services ->
        List.iter (
          fun svc ->
            let svc_node = g#hivex_node_get_child services svc in
            if svc_node <> 0L then (
              (* Disable the service rather than delete the node as it would
               * confuse the uninstaller called from firstboot script. *)
              g#hivex_node_set_value svc_node "Start" 4_L (le32_of_int 4_L)
            )
        ) prl_svcs;

    (* perform the equivalent of DelReg from prl_strg.inf:
     * HKLM, System\CurrentControlSet\Control\Class\{4d36e967-e325-11ce-bfc1-08002be10318}, LowerFilters, 0x00018002, prl_strg
     *)
    let strg_cls = Registry.get_node reg
                        [inspect.i_windows_current_control_set;
                         "Control"; "Class";
                         "{4d36e967-e325-11ce-bfc1-08002be10318}"] in
    match strg_cls with
    | None -> ()
    | Some strg_cls ->
        let lfkey = "LowerFilters" in
        let valueh = g#hivex_node_get_value strg_cls lfkey in
        if valueh <> 0L then (
          let data = g#hivex_value_value valueh in
          let filters = String.nsplit "\000" (Registry.decode_utf16le data) in
          let filters = List.filter (
            fun x -> x <> "prl_strg" && x <> ""
          ) filters in
          let filters = List.map (
            fun x -> Registry.encode_utf16le x ^ "\000\000"
          ) (filters @ [""]) in
          let data = String.concat "" filters in
          g#hivex_node_set_value strg_cls lfkey 7_L data
        )

  and disable_autoreboot reg =
    (* If the guest reboots after a crash, it's hard to see the original
     * error (eg. the infamous 0x0000007B).  Turn off autoreboot.
     *)
    let crash_control =
      Registry.get_node reg [inspect.i_windows_current_control_set;
                             "Control"; "CrashControl"] in
    match crash_control with
    | None -> ()
    | Some crash_control ->
       g#hivex_node_set_value crash_control "AutoReboot" 4_L (le32_of_int 0_L)

  and update_software_hive reg =
    (* Update the SOFTWARE hive.  When this function is called the
     * hive has already been opened as a hivex handle inside
     * guestfs.
     *)

    (* Find the node \Microsoft\Windows\CurrentVersion.  If the node
     * has a key called DevicePath then append the virtio driver
     * path to this key.
     *
     * Note that simply adding the directory to DevicePath doesn't
     * seem to be a 100% reliable way of enabling the drivers.  In
     * particular it does not work for my self-built Windows Server Core
     * 2012 + R2 releases (although that might be an artifact of
     * the way I build them).  In any case I had to add a firstboot
     * batch file which did this single command:
     *
     * %systemroot%\Sysnative\PnPutil -i -a %systemroot%\Drivers\Virtio\*.inf
     *)
    let node =
      Registry.get_node reg ["Microsoft"; "Windows"; "CurrentVersion"] in
    (match node with
     | Some node ->
        let append = Registry.encode_utf16le ";%SystemRoot%\\Drivers\\VirtIO" in
        let values = Array.to_list (g#hivex_node_values node) in
        let rec loop = function
          | [] -> () (* DevicePath not found -- ignore this case *)
          | { G.hivex_value_h = valueh } :: values ->
             let key = g#hivex_value_key valueh in
             if key <> "DevicePath" then
               loop values
             else (
               let data = g#hivex_value_value valueh in
               let len = String.length data in
               let t = g#hivex_value_type valueh in

               (* Only add the appended path if it doesn't exist already. *)
               if String.find data append = -1 then (
                 (* Remove the explicit [\0\0] at the end of the string.
                  * This is the UTF-16LE NUL-terminator.
                  *)
                 let data =
                   if len >= 2 && String.sub data (len-2) 2 = "\000\000" then
                     String.sub data 0 (len-2)
                   else
                     data in

                 (* Append the path and the explicit NUL. *)
                 let data = data ^ append ^ "\000\000" in

                 g#hivex_node_set_value node key t data
               )
             )
        in
        loop values
     | None ->
        warning (f_"could not find registry key \
                    HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion")
    );

    (* If you have trouble installing drivers, try increasing the
     * verboseness by uncommenting this section.
     * https://learn.microsoft.com/en-us/windows-hardware/drivers/install/setting-setupapi-logging-levels
     * The additional logs are written to C:\Windows\INF\setupapi.*.log
     *)
(*
    let node =
      Registry.get_node reg ["Microsoft"; "Windows"; "CurrentVersion";
                             "Setup"] in
    (match node with
     | Some node ->
        g#hivex_node_set_value node "LogLevel" 4_L (le32_of_int 0xff_L)
     | None ->
        warning (f_"could not find registry key \
                    HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Setup")
    );
*)

  and configure_online_disks block_driver =
    (* If there are > 1 disks, run a script which will force Windows
     * to bring them all online.  Windows 2022 will offline non-boot disks
     * where the bus changes as some sort of "security" mitigation.
     * https://issues.redhat.com/browse/RHEL-55837
     * https://issues.redhat.com/browse/MTV-1299
     * https://bugzilla.redhat.com/show_bug.cgi?id=1662286
     *)
    let virtio_installed =
      match block_driver with
      | Inject_virtio_win.Virtio_blk | Virtio_SCSI -> true
      | IDE -> false in
    let more_than_one_disk = List.length source.s_disks > 1 in

    if virtio_installed && more_than_one_disk then (
      let psh_filename = "online-disks" in
      let psh = ref [] in
      let add = List.push_back psh in

      add "# Uncomment this line for lots of debug output.";
      add "# Set-PSDebug -Trace 1";
      add "";
      add "Write-Host \"Online all virtio disks\"";
      add "";
      add "Get-Disk | Where { $_.FriendlyName -like '*VirtIO*' } | % {";
      add "    Write-Host ('  - ' + $_.Number + ': ' + $_.FriendlyName + '(' + [math]::Round($_.Size/1GB,2) + 'GB)')";
      add "    $_ | Set-Disk -IsOffline $false";
      add "    $_ | Set-Disk -IsReadOnly $false";
      add "}";

      (* Install the Powershell script to run late at firstboot. *)
      Firstboot.add_firstboot_powershell g inspect.i_root psh_filename !psh
    )

  and configure_network_interfaces net_driver =
    (* If we were asked to force network interfaces to have particular
     * static IP addresses then it is done here by installing a
     * Powershell script which runs at boot.
     *)
    if static_ips <> [] then (
      let psh_filename = "network-configure" in
      let psh = ref [] in
      let add = List.push_back psh in

      add "# Uncomment this line for lots of debug output.";
      add "# Set-PSDebug -Trace 1";
      add "";

      (* If virtio-net was added to the registry, we must wait for
       * it to be installed at runtime.
       *)
      if net_driver = Virtio_net then (
        add "# Wait for the netkvm (virtio-net) driver to become active.";
        add "$startdate = Get-Date";
        add "$adapters = @()";
        add "While (-Not $adapters -and \
                    $startdate.AddMinutes(5) -gt (Get-Date)) {";
        add "    Start-Sleep -Seconds 5";
        add "    $adapters = Get-NetAdapter -Physical \
                             | Where DriverFileName -eq \"netkvm.sys\"";
        add "    Write-Host \"adapters = '$adapters'\"";
        add "}";
        add "# In the timeout case $ifindex will not be set below.";
        add ""
      );

      List.iter (
        fun { if_mac_addr; if_ip_address; if_default_gateway;
              if_prefix_length; if_nameservers } ->
          add (sprintf "$mac_address = '%s'"
                       (String.replace if_mac_addr ":" "-"));
          add "$ifindex = (Get-NetAdapter -Physical \
                           | Where MacAddress -eq $mac_address).ifIndex";
          add "if ($ifindex) {";

          add "    Write-Host \"setting IP address of adapter at $ifindex\"";

          (* New-NetIPAddress command *)
          let args = ref [] in
          List.push_back args "-InterfaceIndex";
          List.push_back args "$ifindex";
          List.push_back args "-IPAddress";
          List.push_back args (sprintf "'%s'" if_ip_address);
          (match if_default_gateway with
           | None -> ()
           | Some gw ->
              List.push_back args "-DefaultGateway";
              List.push_back args (sprintf "'%s'" gw)
          );
          (match if_prefix_length with
           | None -> ()
           | Some len ->
              List.push_back args "-PrefixLength";
              List.push_back args (string_of_int len)
          );
          let cmd1 = "New-NetIPAddress " ^ String.concat " " !args in
          add ("    " ^ cmd1);

          (* Set-DnsClientServerAddress command *)
          if if_nameservers <> [] then (
            add (sprintf "    Set-DnsClientServerAddress \
                                -InterfaceIndex $ifindex \
                                -ServerAddresses (%s)"
                         (String.concat ","
                            (List.map (sprintf "'%s'") if_nameservers)))
          );
          add "}";
          add ""
      ) static_ips;

      (* Install the Powershell script to run at firstboot.
       *
       * Place it first among the firstboot scripts (RHBZ#1788823).
       *)
      Firstboot.add_firstboot_powershell g inspect.i_root ~prio:2500
        psh_filename !psh
    ) (* static_ips <> [] *)

  and fix_ntfs_heads () =
    (* NTFS hardcodes the number of heads on the drive which created
       it in the filesystem header. Modern versions of Windows
       sensibly ignore it, but both Windows XP and Windows 2000
       require it to be correct in order to boot from the drive. If it
       isn't you get:

       'A disk read error occurred. Press Ctrl+Alt+Del to restart'

       QEMU has some code in block.c:guess_disk_lchs() which on the face
       of it appears to infer the drive geometry from the MBR if it's
       valid. However, my tests have shown that a Windows XP guest
       hosted on both RHEL 5 and F14 requires the heads field in NTFS to
       be the following, based solely on drive size:

       Range                             Heads
       size < 2114445312                 0x40
       2114445312 <= size < 4228374780   0x80
       4228374780 <= size                0xFF

       I have not tested drive sizes less than 1G, which require fewer
       heads, as this limitation applies only to the boot device and it
       is not possible to install XP on a drive this size.

       The following page has good information on the layout of NTFS in
       Windows XP/2000:

       http://mirror.href.com/thestarman/asm/mbr/NTFSBR.htm

       Technet has this:

       http://technet.microsoft.com/en-us/library/cc781134(WS.10).aspx#w2k3tr_ntfs_how_dhao

       however, as this is specific to Windows 2003 it lists location
       0x1A as unused.
    *)
    if inspect.i_major_version < 6 (* is Windows 2000/XP *) then (
      let rootpart = inspect.i_root in

      (* Ignore if the rootpart is something like /dev/sda.  RHBZ#1276540. *)
      if not (g#is_whole_device rootpart) then (
        (* Check that the root device contains NTFS magic. *)
        let magic = g#pread_device rootpart 8 3L in
        if magic = "NTFS    " then (
          (* Get the size of the whole disk containing the root partition. *)
          let rootdev = g#part_to_dev rootpart in (* eg. /dev/sda *)
          let size = g#blockdev_getsize64 rootdev in

          let heads =             (* refer to the table above *)
            if size < 2114445312L then 0x40
            else if size < 4228374780L then 0x80
            else 0xff in

          (* Update NTFS's idea of the number of heads.  This is an
           * unsigned 16 bit little-endian integer, offset 0x1a from the
           * beginning of the partition.
           *)
          let b = Bytes.create 2 in
          Bytes.unsafe_set b 0 (Char.chr heads);
          Bytes.unsafe_set b 1 '\000';
          ignore (g#pwrite_device rootpart (Bytes.to_string b) 0x1a_L)
        )
      )
    )

  and fix_win_esp () =
    let fix_win_uefi_bcd esp_path =
      try
        let bcd_path = "/EFI/Microsoft/Boot/BCD" in
        Registry.with_hive_write g (esp_path ^ bcd_path) (
          (* Remove the 'graphicsmodedisabled' key in BCD *)
          fun reg ->
          let path = ["Objects"; "{9dea862c-5cdd-4e70-acc1-f32b344d4795}";
                      "Elements"; "23000003"] in
          let boot_mgr_default_link =
            match Registry.get_node reg path with
            | None -> raise Not_found
            | Some node -> node in
          let current_boot_entry = g#hivex_value_string (
            g#hivex_node_get_value boot_mgr_default_link "Element") in
          let path = ["Objects"; current_boot_entry; "Elements"; "16000046"] in
          match Registry.get_node reg path with
          | None -> raise Not_found
          | Some graphics_mode_disabled ->
            g#hivex_node_delete_child graphics_mode_disabled
        );
      with
        Not_found -> ()

    and fix_win_uefi_fallback esp_path uefi_arch =
      (* [esp_path] is on NTFS, and therefore it is considered case-sensitive;
       * refer to
       * <https://libguestfs.org/guestfs.3.html#guestfs_case_sensitive_path>.
       * However, the EFI system partition mounted under [esp_path] is FAT32 per
       * UEFI spec, and the Linux vfat driver in the libguestfs appliance treats
       * pathnames case-insensitively. Therefore, we're free to use any case in
       * the ESP-relative pathnames below.
       *)
      let bootmgfw = sprintf "%s/efi/microsoft/boot/bootmgfw.efi" esp_path in
      if g#is_file bootmgfw then
        let bootdir = sprintf "%s/efi/boot" esp_path in
        let fallback = sprintf "%s/boot%s.efi" bootdir uefi_arch in
        if not (g#is_file fallback) || not (g#equal fallback bootmgfw) then (
          info (f_"Fixing UEFI bootloader.");
          g#rm_rf bootdir;
          g#mkdir_p bootdir;
          g#cp_a bootmgfw fallback
        )
    in

    match i_firmware with
    | Firmware.I_BIOS -> ()
    | I_UEFI esp_list ->
      let esp_temp_path = g#mkdtemp "/Windows/Temp/ESP_XXXXXX" in
      let uefi_arch = get_uefi_arch_suffix inspect.i_arch in

      List.iter (
        fun dev_path ->
        g#mount dev_path esp_temp_path;
        fix_win_uefi_bcd esp_temp_path;
        (match uefi_arch with
         | Some uefi_arch -> fix_win_uefi_fallback esp_temp_path uefi_arch
         | None -> ()
        );
        g#umount esp_temp_path;
      ) esp_list;

      g#rmdir esp_temp_path
  in

  do_convert ()

(* Post-conversion steps that run with filesystems unmounted *)
let post_convert (g : G.guestfs) inspect =
  (* Lock down firstboot dir permissions for windows guests *)
  message (f_"Fixing NTFS permissions");
  (* XXX It would be more correct to use g#case_sensitive_path
   * here, but we cannot do that since the guest filesystem is
   * not mounted.  As fixing permissions is best-effort, let's
   * not worry about it.
   *)
  let path = "/Program Files/Guestfs" in
  debug "info: fixing NTFS permissions on %s" path;
  try g#ntfs_chmod inspect.i_root 0o755 path ~recursive:true
  with G.Error msg ->
    warning (f_"ntfs_chmod on %s failed: %s") path msg

module Convert_windows = struct
  let name = "windows"
  let convert = convert
  let post_convert = post_convert
end
