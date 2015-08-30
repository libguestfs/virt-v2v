(* helper-v2v-convert
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
  block_driver : guestcaps_block_type;
  keep_serial_console : bool;
  ks : key_store;
  memsize : int option;
  network_map : Networks.t;
  root_choice : root_choice;
  smp : int option;
  static_ips : static_ip list;
  customize_ops : Customize_cmdline.ops;
}

(* Mountpoint stats, used for free space estimation. *)
type mpstat = {
  mp_dev : string;                      (* Filesystem device (eg. /dev/sda1) *)
  mp_path : string;                     (* Guest mountpoint (eg. /boot) *)
  mp_statvfs : Guestfs.statvfs;         (* Free space stats. *)
  mp_vfs : string;                      (* VFS type (eg. "ext4") *)
}

let rec convert input_disks options source =
  let target_nics = List.map (Networks.map options.network_map) source.s_nics in

  message (f_"Opening the source");
  let g = open_guestfs ~identifier:"v2v" () in
  g#set_program "virt-v2v";
  let memsize =
    match options.memsize with
    | None ->
       (* Default (if [--memsize] option is not used) is to calculate
        * some multiple of the libguestfs default memory size.
        *)
       g#get_memsize () * 2
    | Some memsize -> memsize in
  g#set_memsize memsize;
  let smp =
    match options.smp with
    | None ->
       (* Default (if [--smp] option is not used) is to set the number
        * according to the number of physical CPUs on the host, but
        * limit it because each vCPU consumes guest RAM.  This is
        * necessary to allow parallel mkinitrd which greatly improves
        * performance.
        *)
       min 8 (Sysconf.nr_processors_online ())
    | Some smp -> smp in
  g#set_smp smp;

  (* The network is used by the unconfigure_vmware () function, and the "--key
   * ID:clevis" command line options (if any). *)
  g#set_network true;
  List.iter (
    fun uri ->
      (* NB: Old virt-v2v used copyonread here, when it was using a
       * qcow2 file as overlay.  We MUST NOT use copyonread!  It
       * doesn't do anything if there is no backing chain, but worse
       * than that I observed a huge (33x!) slow down.
       *)
      let socket, export =
        match uri with NBD_URI.Unix (socket, export) ->
          sprintf "unix:%s" socket, Option.value export ~default:"" in
      g#add_drive_opts export
        ~format:"raw" ~protocol:"nbd" ~server:[| socket |]
        ~cachemode:"unsafe" ~discard:"besteffort"
  ) input_disks;

  g#launch ();

  (* Decrypt the disks. *)
  inspect_decrypt g options.ks;

  (* Check (fsck) the filesystems before conversion. *)
  message (f_"Checking filesystem integrity before conversion");
  do_fsck ~before:true g;

  (* Detect firmware. *)
  message (f_"Detecting if this guest uses BIOS or UEFI to boot");
  let i_firmware = Firmware.detect_firmware g in

  (* Inspect the source, choose root and mount up the filesystems. *)
  message (f_"Inspecting the source");
  let root = Choose_root.choose_root options.root_choice g in
  let inspect = Mount_filesystems.mount_filesystems g root in

  (* Detect boot device. *)
  message (f_"Detecting the boot device");
  let target_boot_device = get_target_boot_device g inspect in

  let mpstats = get_mpstats g in
  check_guest_free_space inspect mpstats;

  (* Choose which conversion module we will use. *)
  let (module Conversion_module) =
    match inspect with
    | { i_type = "linux";
        i_distro = ("fedora"
                    | "rhel" | "centos" | "circle" | "scientificlinux"
                    | "redhat-based" | "oraclelinux" | "rocky"
                    | "sles" | "suse-based" | "opensuse"
                    | "altlinux"
                    | "debian" | "ubuntu" | "linuxmint" | "kalilinux") } ->
       (module Convert_linux.Convert_linux : Convert_types.CONVERT)
    | { i_type = "windows" } ->
       (module Convert_windows.Convert_windows : Convert_types.CONVERT)
    | _ ->
       error (f_"virt-v2v is unable to convert this guest type (%s/%s)")
         inspect.i_type inspect.i_distro in
  debug "picked conversion module %s" Conversion_module.name;

  (* Conversion. *)
  let guestcaps =
    do_convert g source inspect i_firmware
      options.block_driver options.keep_serial_console options.static_ips
      Conversion_module.convert in

  (* Run virt-customize options. *)
  Customize_run.run g inspect.i_root options.customize_ops;

  g#umount_all ();

  (* Post-conversion steps that run with filesystems unmounted. *)
  Conversion_module.post_convert g inspect;

  (* Doing fstrim on all the filesystems reduces the transfer size
   * because unused blocks are marked in the overlay and thus do
   * not have to be copied.
   *)
  message (f_"Mapping filesystem data to avoid copying unused and blank areas");
  do_fstrim g inspect;

  (* Check (fsck) the filesystems after conversion. *)
  g#umount_all ();
  message (f_"Checking filesystem integrity after conversion");
  do_fsck g;

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
    get_target_firmware i_firmware guestcaps source output in

  (* Create target metadata file. *)
  let target_meta = { guestcaps; target_buses; target_nics;
                      target_firmware; target_boot_device } in

  (* This is a good place to dump everything we know about the guest. *)
  if verbose () then debug_info source inspect target_meta mpstats;

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

  mpstats

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
        error (f_"not enough free space for conversion on filesystem ‘%s’.  \
                  %.1f MB free < %d MB needed")
          mp_path (mb free_bytes) needed_megabytes
      );
      (* Not all the filesystems have inode counts. *)
      if files > 0L && ffree < needed_inodes then
        error (f_"not enough available inodes for conversion on \
                  filesystem ‘%s’.  %Ld inodes available < %Ld inodes needed")
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
        debug "info: trimming %s" dev;
        try g#fstrim "/"
        with G.Error msg ->
          warning (f_"fstrim on guest filesystem %s failed.  Usually you \
                      can ignore this message.  To find out more read \
                      \"Trimming\" in virt-v2v(1).\n\n\
                      Original message: %s") dev msg
      )
  ) fses

(* Perform fsck before and after conversion.
 *
 * If fsck returns an error then the conversion will fail.  We do not
 * attempt to do any repairs.
 *)
and do_fsck ?(before=false) g =
  let fses = g#list_filesystems () in
  List.iter (function
      | dev, _ when String.starts_with "btrfsvol:" dev ->
         (* Ignore btrfs volumes, since we should see the btrfs device
          * somewhere else in the list and checking that is sufficient.
          *)
         ()

      | dev, "btrfs" ->
         (* btrfs-check would be the obvious thing, but the general
          * opinion seems to be that it's broken, and you should use
          * btrfs scrub instead.
          *)
         Fun.protect ~finally:g#umount_all (
           fun () ->
             g#mount_ro dev "/";
             g#btrfs_scrub_full "/" ~readonly:true;
         );

      | dev, "ext4" ->
         if before then (
           (* Replay and hence repair a dirty log (RHEL-97600) *)
           Fun.protect ~finally:g#umount_all (fun () -> g#mount_ro dev "/");
         );

         g#e2fsck ~forceno:true dev

      | dev, "xfs" ->
         if before then (
           (* xfs_repair cannot replay the dirty log, only the kernel can,
            * so we must first mount then unmount the filesystem, and then
            * we can run xfs_repair.  Unlike what is documented, xfs_repair
            * doesn't return 2 in this case.  Mount r/o is fine as that
            * will still replay the log (RHEL-95365)
            *)
           Fun.protect ~finally:g#umount_all (fun () -> g#mount_ro dev "/");
         );

         (* Must specify the -n flag because we are not attempting to
          * fix the filesystem here.
          *)
         let nomodify = true
         (* xfs_repair runs out of memory in the low memory environment
          * of the appliance unless we limit the amount of memory it will
          * use here.
          *)
         and noprefetch = true
         and maxmem = Int64.of_int (g#get_memsize () / 2) in

         if g#xfs_repair ~maxmem ~noprefetch ~nomodify dev <> 0 then
           error (f_"detected errors on the XFS filesystem on %s") dev

      | _, _ ->
         (* Ignore other filesystem types. *)
         ()
  ) fses

(* Conversion. *)
and do_convert g source inspect i_firmware
               block_driver keep_serial_console interfaces
               convert =
  (* Create the "Converting..." message.  Complicated! *)
  let () =
    let what_guest =
      match inspect.i_product_name, inspect.i_osinfo with
      | "unknown", "unknown" -> s_"the guest"
      | "unknown", osinfo -> sprintf (f_"%s guest") osinfo
      | prod, "unknown" -> prod
      | prod, osinfo -> sprintf "%s (%s)" prod osinfo in

    message (f_"Converting %s to run on KVM") what_guest in

  let guestcaps =
    convert g source inspect i_firmware
            block_driver keep_serial_console interfaces in
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
and get_target_firmware i_firmware guestcaps source output =
  message (f_"Checking if the guest needs BIOS or UEFI to boot");
  let target_firmware =
    match source.s_firmware with
    | BIOS -> TargetBIOS
    | UEFI -> TargetUEFI
    | UnknownFirmware ->
       match i_firmware with
       | I_BIOS -> TargetBIOS
       | I_UEFI _ -> TargetUEFI
  in

  (match target_firmware with
   | TargetBIOS -> ()
   | TargetUEFI -> info (f_"This guest requires UEFI on the target to boot."));

  target_firmware

and get_target_boot_device g inspect =
  with_return (fun {return} ->
    (* We only do it for Linux, as most likely Windows never(?) boots
     * from any drive other than C:.  We can revisit this decision
     * if someone reports a bug.
     *)
    if inspect.i_type <> "linux" then return None;

    (* Look for "GRUB" signature in the boot sector of each disk.
     * If we find it, choose that disk.
     *)
    let devices = g#list_devices () |> Array.to_list in
    let boot_device = List.find_opt (has_grub_signature g) devices in
    let boot_device = Option.map g#device_index boot_device in
    if boot_device <> None then return boot_device;

    (* If that fails, in sane cases, the Grub stage1/boot.img (ie. the boot
     * sector) is always on the same drive as /boot.  So we can just find
     * out where /boot is mounted and use that.
     *)
    get_device_of_boot_filesystem g inspect
  )

and has_grub_signature g dev =
  let boot_sector = g#pread_device dev 512 0_L in
  let r = String.find boot_sector "GRUB" >= 0 in
  debug "has_grub_signature: \"GRUB\" signature on %s? %b" dev r;
  r

and get_device_of_boot_filesystem g inspect =
  try
    let boot_mountpoint = List.assoc "/boot" inspect.i_mountpoints in
    let boot_device = g#part_to_dev boot_mountpoint in
    debug "get_device_of_boot_filesystem: found /boot filesystem on device %s"
      boot_device;
    let boot_device = g#device_index boot_device in
    Some boot_device
  with
  | Not_found -> None
    (* Returned by part_to_dev if the /boot mountpoint is not
     * a partition name.
     *)
  | G.Error msg
       when String.find msg "device name is not a partition" >= 0 -> None
    (* Returned by device_index if the /boot device is not
     * a normal drive name (eg. /dev/mdX).
     *)
  | G.Error msg
       when String.find msg "device not found" >= 0 -> None

(* After conversion we dump as much information about the guest
 * as we can in one place.  Note this is only called when verbose
 * is enabled.
 *)
and debug_info source inspect
               { guestcaps; target_buses; target_nics;
                 target_firmware; target_boot_device }
               mpstats =
  eprintf "info:\n";
  eprintf "%s\n" (string_of_source source);
  eprintf "%s\n" (string_of_inspect inspect);
  eprintf "%s\n" (string_of_guestcaps guestcaps);
  eprintf "%s\n" (string_of_target_buses target_buses);
  eprintf "target firmware: %s\n" (string_of_target_firmware target_firmware);
  eprintf "target boot device: %s\n"
    (match target_boot_device with None -> "" | Some i -> string_of_int i);
  eprintf "target NICs:\n";
  List.iter (fun nic -> eprintf "%s\n" (string_of_source_nic nic))
    target_nics;
  eprintf "mountpoint stats:\n";
  eprintf "%20s %-16s %-16s %-16s %s\n" "" "Size" "Used" "Available" "Use%";
  List.iter debug_mpstat mpstats;
  flush Stdlib.stderr

(* The calculations here are similar to virt-df df/output.c *)
and debug_mpstat { mp_dev = dev; mp_path = path;
                   mp_statvfs = { G.bsize; G.blocks; G.bfree; G.bavail };
                   mp_vfs = vfs } =
  let label = sprintf "%s %s (%s):" dev path vfs
  and size = blocks *^ bsize
  and used = (blocks -^ bfree) *^ bsize
  and avail = bavail *^ bsize
  and percent =
    if blocks <> 0_L then
      100. -. 100. *. (Int64.to_float bfree /. Int64.to_float blocks)
    else
      0. in
  if String.length label > 20 then
    eprintf "%s\n%20s " label ""
  else
    eprintf "%-20s " label;
  eprintf "%-16Ld %-16Ld %-16Ld\n" size used avail;
  eprintf "%20s %-16s %-16s %-16s %.1f%%\n"
    "" (human_size size) (human_size used) (human_size avail) percent
