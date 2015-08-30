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

type options = {
  keep_serial_console : bool;
  ks : key_store;
  network_map : Networks.t;
  root_choice : root_choice;
  static_ips : static_ip list;
}

(* Mountpoint stats, used for free space estimation. *)
type mpstat = {
  mp_dev : string;                      (* Filesystem device (eg. /dev/sda1) *)
  mp_path : string;                     (* Guest mountpoint (eg. /boot) *)
  mp_statvfs : Guestfs.statvfs;         (* Free space stats. *)
  mp_vfs : string;                      (* VFS type (eg. "ext4") *)
}

let rec convert dir options source =
  let target_nics = List.map (Networks.map options.network_map) source.s_nics in

  message (f_"Opening the source");
  let g = open_guestfs ~identifier:"v2v" () in
  g#set_program "virt-v2v";
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
  inspect_decrypt g options.ks;

  (* Inspection - this also mounts up the filesystems. *)
  message (f_"Inspecting the source");
  let inspect = Inspect_source.inspect_source options.root_choice g in

  let mpstats = get_mpstats g in
  check_guest_free_space inspect mpstats;

  (* Conversion. *)
  let guestcaps =
    do_convert g source inspect
      options.keep_serial_console options.static_ips in

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
  let target_meta = { guestcaps; target_buses; target_firmware; target_nics } in

  (* Return inspection data and target metadata. *)
  inspect, target_meta

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
    | Virtio_blk ->
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
