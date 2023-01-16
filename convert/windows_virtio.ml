(* virt-v2v
 * Copyright (C) 2009-2020 Red Hat Inc.
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

open Regedit

open Types

type t = {
  g : Guestfs.guestfs; (** guestfs handle *)

  root : string; (** root of inspection *)

  i_arch : string;
  i_major_version : int;
  i_minor_version : int;
  i_osinfo : string;
  i_product_variant : string;
  i_windows_current_control_set : string;
  i_windows_systemroot : string;
  (** Inspection data needed by this module. *)

  virtio_win : string;
  (** Path to the virtio-win ISO or directory. *)

  was_set : bool;
  (** If the virtio_win path was explicitly set, for example by
      the user setting an environment variable.

      This is used to "show intention" to use virtio-win instead
      of libosinfo.  Although this behaviour is documented, IMHO it has
      always been a bad idea.  We should change this in future to allow
      the user to select where they want to get drivers from. XXX *)
}

type virtio_win_installed = {
  block_driver : guestcaps_block_type;
  net_driver : guestcaps_net_type;
  virtio_rng : bool;
  virtio_balloon : bool;
  isa_pvpanic : bool;
  virtio_socket : bool;
  machine : guestcaps_machine;
  virtio_1_0 : bool;
}

let from_environment g root datadir =
  (* Fail hard if inspection hasn't been done or it's not a Windows
   * guest.  If it happens it indicates an internal error in the
   * calling code.
   *)
  assert (g#inspect_get_type root = "windows");

  let i_arch = g#inspect_get_arch root in
  let i_major_version = g#inspect_get_major_version root in
  let i_minor_version = g#inspect_get_minor_version root in
  let i_osinfo = g#inspect_get_osinfo root in
  let i_product_variant = g#inspect_get_product_variant root in
  let i_windows_current_control_set =
    g#inspect_get_windows_current_control_set root in
  let i_windows_systemroot = g#inspect_get_windows_systemroot root in

  let virtio_win, was_set =
    try Sys.getenv "VIRTIO_WIN", true
    with Not_found ->
      try Sys.getenv "VIRTIO_WIN_DIR" (* old name for VIRTIO_WIN *), true
      with Not_found ->
        let iso = datadir // "virtio-win" // "virtio-win.iso" in
        (if Sys.file_exists iso then iso
         else datadir // "virtio-win"), false in

  { g; root;
    i_arch; i_major_version; i_minor_version; i_osinfo;
    i_product_variant; i_windows_current_control_set; i_windows_systemroot;
    virtio_win; was_set }

let scsi_class_guid = "{4D36E97B-E325-11CE-BFC1-08002BE10318}"
let viostor_legacy_pciid = "VEN_1AF4&DEV_1001&SUBSYS_00021AF4&REV_00"
let viostor_modern_pciid = "VEN_1AF4&DEV_1042&SUBSYS_11001AF4&REV_01"
let vioscsi_legacy_pciid = "VEN_1AF4&DEV_1004&SUBSYS_00081AF4&REV_00"
let vioscsi_modern_pciid = "VEN_1AF4&DEV_1048&SUBSYS_11001AF4&REV_01"

let rec inject_virtio_win_drivers ({ g } as t) reg =
  (* Copy the virtio drivers to the guest. *)
  let driverdir = sprintf "%s/Drivers/VirtIO" t.i_windows_systemroot in
  g#mkdir_p driverdir;

  (* XXX Inelegant hack copied originally from [Convert_windows].
   * We should be able to work this into the code properly later.
   *)
  let machine, virtio_1_0 =
    match t.i_arch with
    | ("i386"|"x86_64") ->
       (try
          (* Fall back to the decision that's based on the year that the OS
           * was released in under three circumstances:
           * - the user specified the location of the Windows virtio drivers
           *   through an environment variable, or
           * - "Libosinfo_utils.get_os_by_short_id" fails to look up the OS,
           *   or
           * - "Libosinfo_utils.best_driver" cannot find any matching driver.
           * In each of these cases, a "Not_found" exception is raised.  This
           * behavior exactly mirrors that of "Windows_virtio.copy_drivers".
           *)
          if t.was_set then raise Not_found;
          let os = Libosinfo_utils.get_os_by_short_id t.i_osinfo in
          let devices = os#get_devices ()
          and drivers = os#get_device_drivers () in
          let best_drv_devs =
            (Libosinfo_utils.best_driver drivers t.i_arch).devices in
          debug "libosinfo internal devices for OS \"%s\":\n%s"
            t.i_osinfo
            (Libosinfo_utils.string_of_osinfo_device_list devices);
          debug "libosinfo \"best driver\" devices for OS \"%s\":\n%s"
            t.i_osinfo
            (Libosinfo_utils.string_of_osinfo_device_list best_drv_devs);
          let { Libosinfo_utils.q35; vio10 } =
            Libosinfo_utils.os_support_of_osinfo_device_list
              (devices @ best_drv_devs) in
          (if q35 then Q35 else I440FX), vio10
        with
        | Not_found ->
           (* Pivot on the year 2007.  Any Windows version from earlier than
            * 2007 should use i440fx, anything 2007 or newer should use q35.
            * Luckily this coincides almost exactly with the release of NT 6.
            *)
           (if t.i_major_version < 6 then I440FX else Q35), true
       )
    | _ -> Virt, true
  in

  if not (copy_drivers t driverdir) then (
      warning (f_"there are no virtio drivers available for this version of Windows (%d.%d %s %s %s).  virt-v2v looks for drivers in %s\n\nThe guest will be configured to use slower emulated devices.")
              t.i_major_version t.i_minor_version t.i_arch
              t.i_product_variant t.i_osinfo t.virtio_win;
      { block_driver = IDE; net_driver = RTL8139;
        virtio_rng = false; virtio_balloon = false;
        isa_pvpanic = false; virtio_socket = false;
        machine; virtio_1_0; }
  )
  else (
    (* Can we install the block driver? *)
    let block : guestcaps_block_type =
      let filenames = ["virtio_blk"; "vrtioblk"; "viostor"] in
      let viostor_driver = try (
        Some (
          List.find (
            fun driver_file ->
              let source = driverdir // driver_file ^ ".sys" in
              g#exists source
          ) filenames
        )
      ) with Not_found -> None in
      match viostor_driver with
      | None ->
        warning (f_"there is no virtio block device driver for this version of Windows (%d.%d %s).  virt-v2v looks for this driver in %s\n\nThe guest will be configured to use a slower emulated device.")
                t.i_major_version t.i_minor_version
                t.i_arch t.virtio_win;
        IDE

      | Some driver_name ->
        (* Block driver needs tweaks to allow booting; the rest is set up by PnP
         * manager *)
        let source = driverdir // (driver_name ^ ".sys") in
        let target = sprintf "%s/system32/drivers/%s.sys"
                             t.i_windows_systemroot driver_name in
        let target = g#case_sensitive_path target in
        g#cp source target;
        add_guestor_to_registry t reg driver_name viostor_legacy_pciid;
        add_guestor_to_registry t reg driver_name viostor_modern_pciid;
        Virtio_blk in

    (* Can we install the virtio-net driver? *)
    let net : guestcaps_net_type =
      let filenames = ["virtio_net.inf"; "netkvm.inf"] in
      let has_netkvm =
        List.exists (
          fun driver_file -> g#exists (driverdir // driver_file)
        ) filenames in
      if not has_netkvm then (
        warning (f_"there is no virtio network driver for this version of Windows (%d.%d %s).  virt-v2v looks for this driver in %s\n\nThe guest will be configured to use a slower emulated device.")
                t.i_major_version t.i_minor_version
                t.i_arch t.virtio_win;
        RTL8139
      )
      else
        Virtio_net in

    (* The "fwcfg" driver binds the fw_cfg device for real, and provides three
     * files -- ".cat", ".inf", ".sys".  (Possibly ".pdb" too.)
     *
     * The "qemufwcfg" driver is only a stub driver; it placates Device Manager
     * (hides the "unknown device" question mark) but does not actually drive
     * the fw_cfg device.  It provides two files only -- ".cat", ".inf".
     *
     * These drivers conflict with each other (RHBZ#2151752).  If we've copied
     * both (either from libosinfo of virtio-win), let "fwcfg" take priority:
     * remove "qemufwcfg".
     *)
    if g#exists (driverdir // "fwcfg.inf") &&
       g#exists (driverdir // "qemufwcfg.inf") then (
      debug "windows: skipping qemufwcfg stub driver in favor of fwcfg driver";
      Array.iter g#rm (g#glob_expand (driverdir // "qemufwcfg.*"))
    );

    (* Did we install the miscellaneous drivers? *)
    { block_driver = block;
      net_driver = net;
      virtio_rng = g#exists (driverdir // "viorng.inf");
      virtio_balloon = g#exists (driverdir // "balloon.inf");
      isa_pvpanic = g#exists (driverdir // "pvpanic.inf");
      virtio_socket = g#exists (driverdir // "viosock.inf");
      machine; virtio_1_0;
    }
  )

and inject_qemu_ga t =
  let msi_files = copy_qemu_ga t in
  if msi_files <> [] then
    configure_qemu_ga t msi_files;
  msi_files <> [] (* return true if we found some qemu-ga MSI files *)

and add_guestor_to_registry t ((g, root) as reg) drv_name drv_pciid =
  let ddb_node = g#hivex_node_get_child root "DriverDatabase" in

  let regedits =
    if ddb_node = 0L then
      cdb_regedits t drv_name drv_pciid
    else
      ddb_regedits t drv_name drv_pciid in

  let drv_sys_path = sprintf "system32\\drivers\\%s.sys" drv_name in
  let common_regedits = [
      [ t.i_windows_current_control_set; "Services"; drv_name ],
      [ "Type", REG_DWORD 0x1_l;
        "Start", REG_DWORD 0x0_l;
        "Group", REG_SZ "SCSI miniport";
        "ErrorControl", REG_DWORD 0x1_l;
        "ImagePath", REG_EXPAND_SZ drv_sys_path ];
  ] in

  reg_import reg (regedits @ common_regedits)

and cdb_regedits t drv_name drv_pciid =
  (* See http://rwmj.wordpress.com/2010/04/30/tip-install-a-device-driver-in-a-windows-vm/
   * NB: All these edits are in the HKLM\SYSTEM hive.  No other
   * hive may be modified here.
   *)
  [
    [ t.i_windows_current_control_set;
      "Control"; "CriticalDeviceDatabase";
      "PCI#" ^ drv_pciid ],
    [ "Service", REG_SZ drv_name;
      "ClassGUID", REG_SZ scsi_class_guid ];
  ]

and ddb_regedits inspect drv_name drv_pciid =
  (* Windows >= 8 doesn't use the CriticalDeviceDatabase.  Instead
   * one must add keys into the DriverDatabase.
   *)

  let drv_inf = "guestor.inf" in
  let drv_inf_label = drv_inf ^ "_tmp" in
  let drv_config = "guestor_conf" in

  [
    [ "DriverDatabase"; "DriverInfFiles"; drv_inf ],
    [ "", REG_MULTI_SZ [ drv_inf_label ];
      "Active", REG_SZ drv_inf_label;
      "Configurations", REG_MULTI_SZ [ drv_config ] ];

    [ "DriverDatabase"; "DeviceIds"; "PCI"; drv_pciid ],
    [ drv_inf, REG_BINARY "\x01\xff\x00\x00" ];

    [ "DriverDatabase"; "DriverPackages"; drv_inf_label ],
    [ "Version", REG_BINARY ("\x00\xff\x09\x00\x00\x00\x00\x00" ^
                             "\x7b\xe9\x36\x4d\x25\xe3\xce\x11" ^
                             "\xbf\xc1\x08\x00\x2b\xe1\x03\x18" ^
                             (String.make 24 '\x00')) ];
    (* Version is necessary for Windows-Kernel-Pnp in w10/w2k16 *)

    [ "DriverDatabase"; "DriverPackages"; drv_inf_label;
      "Configurations"; drv_config ],
    [ "ConfigFlags", REG_DWORD 0_l;
      "Service", REG_SZ drv_name ];

    [ "DriverDatabase"; "DriverPackages"; drv_inf_label;
      "Descriptors"; "PCI"; drv_pciid ],
    [ "Configuration", REG_SZ drv_config ];
  ]

(* Copy the matching drivers to the driverdir; return true if any have
 * been copied.
 *)
and copy_drivers t driverdir =
  (not t.was_set && [] <> copy_from_libosinfo t driverdir) ||
    [] <> copy_from_virtio_win t "/" driverdir
            (virtio_iso_path_matches_guest_os t)
      (fun () ->
        error (f_"root directory ‘/’ is missing from the virtio-win directory or ISO.\n\nThis should not happen and may indicate that virtio-win or virt-v2v is broken in some way.  Please report this as a bug with a full debug log."))

and copy_qemu_ga t =
  copy_from_virtio_win t "/" "/" (virtio_iso_path_matches_qemu_ga t)
    (fun () ->
      error (f_"root directory ‘/’ is missing from the virtio-win directory or ISO.\n\nThis should not happen and may indicate that virtio-win or virt-v2v is broken in some way.  Please report this as a bug with a full debug log."))

(* Copy all files from virtio_win directory/ISO located in [srcdir]
 * subdirectory and all its subdirectories to the [destdir]. The directory
 * hierarchy is not preserved, meaning all files will be directly in [destdir].
 * The file list is filtered based on [filter] function.
 *
 * If [srcdir] is missing from the ISO then [missing ()] is called
 * which might give a warning or error.
 *
 * Returns list of copied files.
 *)
and copy_from_virtio_win ({ g } as t) srcdir destdir filter missing =
  let ret = ref [] in
  if is_directory t.virtio_win then (
    debug "windows: copy_from_virtio_win: guest tools source directory %s"
      t.virtio_win;

    let dir = t.virtio_win // srcdir in
    if not (is_directory dir) then missing ()
    else (
      let cmd = sprintf "cd %s && find -L -type f" (quote dir) in
      let paths = external_command cmd in
      List.iter (
        fun path ->
          if filter path then (
            let source = dir // path in
            let target_name = String.lowercase_ascii (Filename.basename path) in
            let target = destdir // target_name in
            debug "windows: copying guest tools bits: 'host:%s' -> '%s'"
                  source target;

            g#write target (read_whole_file source);
            List.push_front target_name ret
          )
      ) paths
    )
  )
  else if is_regular_file t.virtio_win || is_block_device t.virtio_win then (
    debug "windows: copy_from_virtio_win: guest tools source ISO %s"
      t.virtio_win;

    let g2 =
      try
        let g2 = open_guestfs ~identifier:"virtio_win" () in
        g2#add_drive_opts t.virtio_win ~readonly:true;
        g2#launch ();
        g2
      with Guestfs.Error msg ->
        error (f_"%s: cannot open virtio-win ISO file: %s") t.virtio_win msg in
    (* Note we are mounting this as root on the *second*
     * handle, not the main handle containing the guest.
     *)
    g2#mount_ro "/dev/sda" "/";
    let srcdir = "/" ^ srcdir in
    if not (g2#is_dir srcdir) then missing ()
    else (
      let paths = g2#find srcdir in
      Array.iter (
        fun path ->
          let source = srcdir ^ "/" ^ path in
          if g2#is_file source ~followsymlinks:false && filter path then (
            let target_name = String.lowercase_ascii (Filename.basename path) in
            let target = destdir ^ "/" ^ target_name in
            debug "windows: copying guest tools bits: '%s:%s' -> '%s'"
              t.virtio_win path target;

            g#write target (g2#read_file source);
            List.push_front target_name ret
          )
      ) paths;
    );
    g2#close()
  );
  !ret

(* Given a path of a file relative to the root of the directory tree
 * with virtio-win drivers, figure out if it's suitable for the
 * specific Windows flavor of the current guest.
 *)
and virtio_iso_path_matches_guest_os t path =
  let { i_major_version = os_major; i_minor_version = os_minor;
        i_arch = arch; i_product_variant = os_variant;
        i_osinfo = osinfo } = t in
  try
    (* Lowercased path, since the ISO may contain upper or lowercase path
     * elements.
     *)
    let lc_path = String.lowercase_ascii path in

    (* Using the full path, work out what version of Windows
     * this driver is for.  Paths can be things like:
     * "NetKVM/2k12R2/amd64/netkvm.sys" or
     * "./drivers/amd64/Win2012R2/netkvm.sys".
     * Note we check lowercase paths.
     *)
    let pathelem elem = String.find lc_path ("/" ^ elem ^ "/") >= 0 in
    let p_arch =
      if pathelem "x86" || pathelem "i386" then "i386"
      else if pathelem "amd64" then "x86_64"
      else raise Not_found in

    let is_client os_variant = os_variant = "Client"
    and not_client os_variant = os_variant <> "Client"
    and any_variant os_variant = true
    and any_osinfo osinfo = true in
    let p_os_major, p_os_minor, match_os_variant, match_osinfo =
      if pathelem "xp" || pathelem "winxp" then
        (5, 1, any_variant, any_osinfo)
      else if pathelem "2k3" || pathelem "win2003" then
        (5, 2, any_variant, any_osinfo)
      else if pathelem "vista" then
        (6, 0, is_client, any_osinfo)
      else if pathelem "2k8" || pathelem "win2008" then
        (6, 0, not_client, any_osinfo)
      else if pathelem "w7" || pathelem "win7" then
        (6, 1, is_client, any_osinfo)
      else if pathelem "2k8r2" || pathelem "win2008r2" then
        (6, 1, not_client, any_osinfo)
      else if pathelem "w8" || pathelem "win8" then
        (6, 2, is_client, any_osinfo)
      else if pathelem "2k12" || pathelem "win2012" then
        (6, 2, not_client, any_osinfo)
      else if pathelem "w8.1" || pathelem "win8.1" then
        (6, 3, is_client, any_osinfo)
      else if pathelem "2k12r2" || pathelem "win2012r2" then
        (6, 3, not_client, any_osinfo)
      else if pathelem "w10" || pathelem "win10" then
        (10, 0, is_client, ((=) "win10"))
      else if pathelem "w11" || pathelem "win11" then
        (10, 0, is_client, ((=) "win11"))
      else if pathelem "2k16" || pathelem "win2016" then
        (10, 0, not_client, ((=) "win2k16"))
      else if pathelem "2k19" || pathelem "win2019" then
        (10, 0, not_client, ((=) "win2k19"))
      else if pathelem "2k22" || pathelem "win2022" then
        (10, 0, not_client, ((=) "win2k22"))
      else
        raise Not_found in

    arch = p_arch && os_major = p_os_major && os_minor = p_os_minor &&
    match_os_variant os_variant &&
    match_osinfo osinfo

  with Not_found -> false

(* Given a path of a file relative to the root of the directory tree
 * with virtio-win drivers, figure out if it's suitable for the
 * specific Windows flavor of the current guest.
 *)
and virtio_iso_path_matches_qemu_ga t path =
  (* Lowercased path, since the ISO may contain upper or lowercase path
   * elements.
   *)
  let lc_name = String.lowercase_ascii (Filename.basename path) in
  match t.i_arch, lc_name with
  | ("i386", "qemu-ga-x86.msi")
  | ("i386", "qemu-ga-i386.msi")
  | ("i386", "rhev-qga.msi")
  | ("x86_64", "qemu-ga-x64.msi")
  | ("x86_64", "qemu-ga-x86_64.msi")
  | ("x86_64", "rhev-qga64.msi") -> true
  | _ -> false

(* Look up in libosinfo for the OS, and copy all the locally
 * available files specified as drivers for that OS to the [destdir].
 *
 * This function does nothing in case either:
 * - the osinfo short ID is not found in the libosinfo DB
 * - the OS does not have any driver for the architecture of the guest
 * - the location of the drivers is not a local directory
 *
 * Files that do not exist are silently skipped.
 *
 * Returns list of copied files.
 *)
and copy_from_libosinfo { g; i_osinfo; i_arch } destdir =
  try
    let os = Libosinfo_utils.get_os_by_short_id i_osinfo in
    let drivers = os#get_device_drivers () in
    let driver = Libosinfo_utils.best_driver drivers i_arch in
    let uri = Xml.parse_uri driver.Libosinfo.location in
    let basedir =
      match uri.Xml.uri_path with
      | Some p -> p
      | None -> assert false in
    List.filter_map (
      fun f ->
        let source = basedir // f in
        if not (Sys.file_exists source) then
          None
        else (
          let target_name = String.lowercase_ascii (Filename.basename f) in
          let target = destdir ^ "/" ^ target_name in
          debug "windows: copying guest tools bits (via libosinfo): 'host:%s' -> '%s'"
                source target;

          g#write target (read_whole_file source);
          Some target_name
        )
    ) driver.Libosinfo.files
  with Not_found -> []

and configure_qemu_ga t files =
  List.iter (
    fun msi_path ->
      (* Windows is a trashfire.
       * https://stackoverflow.com/a/18730884
       * https://bugzilla.redhat.com/show_bug.cgi?id=1895323
       *)
      let psh_script = ref [] in
      let add = List.push_back psh_script in

      add "# Uncomment this line for lots of debug output.";
      add "# Set-PSDebug -Trace 2";
      add "";
      add "Write-Host Removing any previously scheduled qemu-ga installation";
      add "schtasks.exe /Delete /TN Firstboot-qemu-ga /F";
      add "";
      add (sprintf
             "Write-Host Scheduling delayed installation of qemu-ga from %s"
             msi_path);
      add "$d = (get-date).AddSeconds(120)";
      add "$dtfinfo = [System.Globalization.DateTimeFormatInfo]::CurrentInfo";
      add "$sdp = $dtfinfo.ShortDatePattern";
      add "$sdp = $sdp -replace 'y+', 'yyyy'";
      add "$sdp = $sdp -replace 'M+', 'MM'";
      add "$sdp = $sdp -replace 'd+', 'dd'";
      add "schtasks.exe /Create /SC ONCE `";
      add "  /ST $d.ToString('HH:mm') /SD $d.ToString($sdp) `";
      add "  /RU SYSTEM /TN Firstboot-qemu-ga `";
      add (sprintf "  /TR \"C:\\%s /forcerestart /qn /l+*vx C:\\%s.log\""
             msi_path msi_path);

      Firstboot.add_firstboot_powershell t.g t.root
        (sprintf "install-%s.ps1" msi_path) !psh_script;
  ) files
