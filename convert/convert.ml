(* helper-v2v-convert
 * Copyright (C) 2009-2021 Red Hat Inc.
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
open Unix

open Std_utils
open Tools_utils
open Unix_utils
open Common_gettext.Gettext
open Getopt.OptionName

open Types
open Utils

module G = Guestfs

(* Matches --mac command line parameters. *)
let mac_re = PCRE.compile ~anchored:true "([[:xdigit:]]{2}:[[:xdigit:]]{2}:[[:xdigit:]]{2}:[[:xdigit:]]{2}:[[:xdigit:]]{2}:[[:xdigit:]]{2}):(network|bridge|ip):(.*)"
let mac_ip_re = PCRE.compile ~anchored:true "([[:xdigit:]]|:|\\.)+"

(* Mountpoint stats, used for free space estimation. *)
type mpstat = {
  mp_dev : string;                      (* Filesystem device (eg. /dev/sda1) *)
  mp_path : string;                     (* Guest mountpoint (eg. /boot) *)
  mp_statvfs : Guestfs.statvfs;         (* Free space stats. *)
  mp_vfs : string;                      (* VFS type (eg. "ext4") *)
}

let rec main () =
  let set_string_option_once optname optref arg =
    match !optref with
    | Some _ ->
       error (f_"%s option used more than once on the command line") optname
    | None ->
       optref := Some arg
  in

  let network_map = Networks.create () in
  let static_ips = ref [] in
  let rec add_network str =
    match String.split ":" str with
    | "", "" ->
       error (f_"invalid -n/--network parameter")
    | out, "" | "", out ->
       Networks.add_default_network network_map out
    | in_, out ->
       Networks.add_network network_map in_ out
  and add_bridge str =
    match String.split ":" str with
    | "", "" ->
       error (f_"invalid -b/--bridge parameter")
    | out, "" | "", out ->
       Networks.add_default_bridge network_map out
    | in_, out ->
       Networks.add_bridge network_map in_ out
  and add_mac str =
    if not (PCRE.matches mac_re str) then
      error (f_"cannot parse --mac \"%s\" parameter") str;
    let mac = PCRE.sub 1 and out = PCRE.sub 3 in
    match PCRE.sub 2 with
    | "network" ->
       Networks.add_mac network_map mac Network out
    | "bridge" ->
       Networks.add_mac network_map mac Bridge out
    | "ip" ->
       (match String.nsplit "," out with
        | [] -> error (f_"invalid --mac ip option")
        | [ip] -> add_static_ip mac ip None None []
        | [ip; gw] -> add_static_ip mac ip (Some gw) None []
        | ip :: gw :: len :: nameservers ->
           add_static_ip mac ip (Some gw) (Some len) nameservers
       )
    | _ -> assert false
  and add_static_ip if_mac_addr if_ip_address if_default_gateway
                    if_prefix_length_str if_nameservers =
    (* Check the IP addresses and prefix length are sensible.  This
     * is only a very simple test that they are sane, since IP addresses
     * come in too many valid forms to check thoroughly.
     *)
    let rec error_unless_ip_addr what addr =
      if not (PCRE.matches mac_ip_re addr) then
        error (f_"cannot parse --mac ip %s: doesn’t look like “%s” is an IP address") what addr
    in
    error_unless_ip_addr "ipaddr" if_ip_address;
    Option.may (error_unless_ip_addr "gw") if_default_gateway;
    List.iter (error_unless_ip_addr "nameserver") if_nameservers;
    let if_prefix_length =
      match if_prefix_length_str with
      | None -> None
      | Some len ->
         let len =
           try int_of_string len with
           | Failure _ -> error (f_"cannot parse --mac ip prefix length field as an integer: %s") len in
         if len < 0 || len > 128 then
           error (f_"--mac ip prefix length field is out of range");
         Some len in
    List.push_back static_ips
      { if_mac_addr; if_ip_address; if_default_gateway;
        if_prefix_length; if_nameservers }
  in

  let root_choice = ref AskRoot in
  let set_root_choice = function
    | "ask" -> root_choice := AskRoot
    | "single" -> root_choice := SingleRoot
    | "first" -> root_choice := FirstRoot
    | dev when String.is_prefix dev "/dev/" -> root_choice := RootDev dev
    | s ->
      error (f_"unknown --root option: %s") s
  in

  let keep_serial_console = ref true in
  let output_name = ref None in

  (* Parse the command line. *)
  let args = ref [] in
  let anon_fun s = List.push_front s args in
  let argspec = [
    [ S 'b'; L"bridge" ], Getopt.String ("in:out", add_bridge),
                                    s_"Map bridge ‘in’ to ‘out’";
    [ L"mac" ],      Getopt.String ("mac:network|bridge|ip:out", add_mac),
                                    s_"Map NIC to network or bridge or assign static IP";
    [ S 'n'; L"network" ], Getopt.String ("in:out", add_network),
                                    s_"Map network ‘in’ to ‘out’";
    [ M"on" ],       Getopt.String ("name", set_string_option_once "-on" output_name),
                                    s_"Rename guest when converting";
    [ L"remove-serial-console" ],
                     Getopt.Clear keep_serial_console,
                                    s_"Remove Linux serial console";
    [ L"root" ],     Getopt.String ("ask|... ", set_root_choice),
                                    s_"How to choose root filesystem";
  ] in

  let usage_msg =
    sprintf (f_"\
%s: helper to convert a guest to run on KVM

helper-v2v-convert V2VDIR
")
      prog in
  let opthandle =
    create_standard_options argspec ~anon_fun ~program_name:true usage_msg in
  Getopt.parse opthandle.getopt;

  (* Dereference the arguments. *)
  let args = List.rev !args in
  let keep_serial_console = !keep_serial_console in
  let output_name = !output_name in
  let root_choice = !root_choice in
  let static_ips = !static_ips in

  (* Check correct number of anon args were passed. *)
  let dir =
    match args with
    | [] -> error (f_"the first parameter must be the V2V directory")
    | [dir] -> dir
    | _ -> error (f_"too many anon args passed to %s") prog in

  (* The v2v directory must exist. *)
  if not (is_directory dir) then
    error (f_"%s does not exist or is not a directory") dir;

  (* Read the source metadata written by the input helper. *)
  let source =
    with_open_in (dir // "source") (
      fun chan ->
        let ver = input_value chan in
        assert (ver = Utils.metaversion);
        (input_value chan : Types.source)
    ) in

  let output_name = Option.default source.s_name output_name in
  let target_nics = List.map (Networks.map network_map) source.s_nics in

  message (f_"Opening the source");
  let g = open_guestfs ~identifier:"v2v" () in
  g#set_memsize (g#get_memsize () * 2);
  (* Setting the number of vCPUs allows parallel mkinitrd, but make
   * sure this is not too large because each vCPU consumes guest RAM.
   *)
  g#set_smp (min 8 (Sysconf.nr_processors_online ()));
  (* The network is only used by the unconfigure_vmware () function. *)
  g#set_network true;
  List.iter (
    fun { s_disk_id = i } ->
      (* NB: Old virt-v2v used copyonread here, when it was using a
       * qcow2 file as overlay.  We MUST NOT use copyonread!  It
       * doesn't do anything if there is no backing chain, but worse
       * than that I observed a huge (33x!) slow down.
       *)
      let socket = sprintf "unix:%s/in%d" dir i in
      g#add_drive_opts ""
        ~format:"raw" ~protocol:"nbd" ~server:[| socket |]
        ~cachemode:"unsafe" ~discard:"besteffort"
  ) source.s_disks;

  g#launch ();

  (* Decrypt the disks. *)
  inspect_decrypt g opthandle.ks;

  (* Inspection - this also mounts up the filesystems. *)
  message (f_"Inspecting the source");
  let inspect = Inspect_source.inspect_source root_choice g in

  with_open_out (dir // "inspect") (
    fun chan ->
      output_value chan Utils.metaversion;
      output_value chan inspect
  );

  let mpstats = get_mpstats g in
  check_guest_free_space inspect mpstats;

  (* Conversion. *)
  let guestcaps =
    do_convert g source inspect keep_serial_console static_ips in

  g#umount_all ();

  (* Doing fstrim on all the filesystems reduces the transfer size
   * because unused blocks are marked in the overlay and thus do
   * not have to be copied.
   *)
  message (f_"Mapping filesystem data to avoid copying unused and blank areas");
  do_fstrim g inspect;

  message (f_"Closing the overlay");
  g#umount_all ();
  g#shutdown ();
  g#close ();

  (* Prepare the target metadata. *)
  message (f_"Assigning disks to buses");
  let target_buses =
    Target_bus_assignment.target_bus_assignment
      source.s_disks source.s_removables guestcaps in
  debug "%s" (string_of_target_buses target_buses);

  let target_firmware =
    get_target_firmware inspect guestcaps source output in

  (* Create target metadata file. *)
  let target_meta = { guestcaps; output_name;
                      target_buses; target_firmware; target_nics } in
  with_open_out (dir // "target_meta") (
    fun chan ->
      output_value chan Utils.metaversion;
      output_value chan target_meta
  )

(* Collect statvfs information from the guest mountpoints. *)
and get_mpstats g =
  let mpstats = List.map (
    fun (dev, path) ->
      let statvfs = g#statvfs path in
      let vfs = g#vfs_type dev in
      { mp_dev = dev; mp_path = path; mp_statvfs = statvfs; mp_vfs = vfs }
  ) (g#mountpoints ()) in

  if verbose () then (
    (* This is useful for debugging speed / fstrim issues. *)
    eprintf "mpstats:\n";
    List.iter (print_mpstat Stdlib.stderr) mpstats
  );

  mpstats

and print_mpstat chan { mp_dev = dev; mp_path = path;
                        mp_statvfs = s; mp_vfs = vfs } =
  fprintf chan "mountpoint statvfs %s %s (%s):\n" dev path vfs;
  fprintf chan "  bsize=%Ld blocks=%Ld bfree=%Ld bavail=%Ld\n"
    s.Guestfs.bsize s.Guestfs.blocks s.Guestfs.bfree s.Guestfs.bavail

(* Conversion can fail if there is no space on the guest filesystems
 * (RHBZ#1139543).  To avoid this situation, check there is some
 * headroom.  Mainly we care about the root filesystem.
 *
 * Also make sure filesystems have available inodes. (RHBZ#1764569)
 *)
and check_guest_free_space inspect mpstats =
  message (f_"Checking for sufficient free disk space in the guest");

  (* Check whether /boot has its own mount point. *)
  let has_boot = List.exists (fun { mp_path } -> mp_path = "/boot") mpstats in
  let is_windows = inspect.i_distro = "windows" in

  let needed_megabytes_for_mp = function
    (* We usually regenerate the initramfs, which has a
     * typical size of 20-30MB.  Hence:
     *)
    | "/boot" | "/" when not has_boot && not is_windows -> 50
    (* Both Linux and Windows require installation of files,
     * device drivers and guest agents.
     * https://bugzilla.redhat.com/1949147
     * https://bugzilla.redhat.com/1764569#c16
     *)
    | "/" -> 100
    (* For everything else, just make sure there is some free space. *)
    | _ -> 10
  in

  (* Reasonable headroom for conversion operations. *)
  let needed_inodes = 100L in

  List.iter (
    fun { mp_path; mp_statvfs = { G.bfree; bsize; files; ffree } } ->
      (* bfree = free blocks for root user *)
      let free_bytes = bfree *^ bsize in
      let needed_megabytes = needed_megabytes_for_mp mp_path in
      let needed_bytes = Int64.of_int needed_megabytes *^ 1024L *^ 1024L in
      if free_bytes < needed_bytes then (
        let mb i = Int64.to_float i /. 1024. /. 1024. in
        error (f_"not enough free space for conversion on filesystem ‘%s’.  %.1f MB free < %d MB needed")
          mp_path (mb free_bytes) needed_megabytes
      );
      (* Not all the filesystems have inode counts. *)
      if files > 0L && ffree < needed_inodes then
        error (f_"not enough available inodes for conversion on filesystem ‘%s’.  %Ld inodes available < %Ld inodes needed")
          mp_path ffree needed_inodes
  ) mpstats

(* Perform the fstrim. *)
and do_fstrim g inspect =
  (* Get all filesystems. *)
  let fses = g#list_filesystems () in

  let fses = List.filter_map (
    function (_, ("unknown"|"swap")) -> None | (dev, _) -> Some dev
  ) fses in

  (* Trim the filesystems. *)
  List.iter (
    fun dev ->
      g#umount_all ();
      let mounted =
        try g#mount_options "discard" dev "/"; true
        with G.Error _ -> false in

      if mounted then (
        try g#fstrim "/"
        with G.Error msg ->
          warning (f_"fstrim on guest filesystem %s failed.  Usually you can ignore this message.  To find out more read \"Trimming\" in virt-v2v(1).\n\nOriginal message: %s") dev msg
      )
  ) fses

(* Conversion. *)
and do_convert g source inspect keep_serial_console interfaces =
  (match inspect.i_product_name with
  | "unknown" ->
    message (f_"Converting the guest to run on KVM")
  | prod ->
    message (f_"Converting %s to run on KVM") prod
  );

  let convert, conversion_name =
    match inspect with
    | { i_type = "linux";
        i_distro = ("fedora"
                    | "rhel" | "centos" | "scientificlinux" | "redhat-based"
                    | "oraclelinux"
                    | "sles" | "suse-based" | "opensuse"
                    | "altlinux"
                    | "debian" | "ubuntu" | "linuxmint" | "kalilinux") } ->
       Convert_linux.convert, "linux"
    | { i_type = "windows" } ->
       Convert_windows.convert, "windows"
    | _ ->
       error (f_"virt-v2v is unable to convert this guest type (%s/%s)")
         inspect.i_type inspect.i_distro in
  debug "picked conversion module %s" conversion_name;
  let guestcaps =
    convert g source inspect keep_serial_console interfaces in
  debug "%s" (string_of_guestcaps guestcaps);

  (* Did we manage to install virtio drivers? *)
  if not (quiet ()) then (
    match guestcaps.gcaps_block_bus with
    | Virtio_blk | Virtio_SCSI ->
        info (f_"This guest has virtio drivers installed.")
    | IDE ->
        info (f_"This guest does not have virtio drivers installed.")
  );

  guestcaps

(* Does the guest require UEFI on the target? *)
and get_target_firmware inspect guestcaps source output =
  message (f_"Checking if the guest needs BIOS or UEFI to boot");
  let target_firmware =
    match source.s_firmware with
    | BIOS -> TargetBIOS
    | UEFI -> TargetUEFI
    | UnknownFirmware ->
       match inspect.i_firmware with
       | I_BIOS -> TargetBIOS
       | I_UEFI _ -> TargetUEFI
  in

  (match target_firmware with
   | TargetBIOS -> ()
   | TargetUEFI -> info (f_"This guest requires UEFI on the target to boot."));

  target_firmware

let () = run_main_and_handle_errors main
