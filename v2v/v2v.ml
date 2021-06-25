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

open Unix
open Printf

open C_utils
open Std_utils
open Tools_utils
open Unix_utils
open Common_gettext.Gettext

open Types
open Utils

open Cmdline

module G = Guestfs
module IntSet = Set.Make(struct let compare = compare type t = int end)

(* Conversion mode, either normal (copying) or [--in-place]. *)
type conversion_mode =
  | Copying of overlay list
  | In_place

(* Mountpoint stats, used for free space estimation. *)
type mpstat = {
  mp_dev : string;                      (* Filesystem device (eg. /dev/sda1) *)
  mp_path : string;                     (* Guest mountpoint (eg. /boot) *)
  mp_statvfs : Guestfs.statvfs;         (* Free space stats. *)
  mp_vfs : string;                      (* VFS type (eg. "ext4") *)
}

let () = Random.self_init ()

let sum = List.fold_left (+^) 0L

let rec main () =
  (* Handle the command line. *)
  let cmdline, input, output = parse_cmdline () in

  (* Print the version, easier than asking users to tell us. *)
  debug "%s: %s %s (%s)"
        prog Config.package_name Config.package_version_full
        Config.host_cpu;

  (* Print the libvirt version if debugging. *)
  if verbose () then (
    let major, minor, release = Libvirt_utils.libvirt_get_version () in
    debug "libvirt version: %d.%d.%d" major minor release
  );

  (* Perform pre-flight checks on the input and output objects. *)
  input#precheck ();
  output#precheck ();

  let source = open_source cmdline input in
  let output_name = Option.default source.s_name cmdline.output_name in
  let target_nics = List.map (Networks.map cmdline.network_map) source.s_nics in

  let conversion_mode =
    if not cmdline.in_place then (
      check_host_free_space ();
      let overlays = create_overlays source.s_disks in
      Copying overlays
    )
    else In_place in

  (match conversion_mode with
   | Copying _ -> message (f_"Opening the overlay")
   | In_place -> message (f_"Opening the source VM")
  );

  let g = open_guestfs ~identifier:"v2v" () in
  g#set_memsize (g#get_memsize () * 14 / 5);
  (* Setting the number of vCPUs allows parallel mkinitrd, but make
   * sure this is not too large because each vCPU consumes guest RAM.
   *)
  g#set_smp (min 8 (Sysconf.nr_processors_online ()));
  (* The network is only used by the unconfigure_vmware () function. *)
  g#set_network true;
  (match conversion_mode with
   | Copying overlays -> populate_overlays g overlays
   | In_place -> populate_disks g source.s_disks
  );

  g#launch ();

  (* Decrypt the disks. *)
  inspect_decrypt g cmdline.ks;

  (* Inspection - this also mounts up the filesystems. *)
  (match conversion_mode with
   | Copying _ -> message (f_"Inspecting the overlay")
   | In_place -> message (f_"Inspecting the source VM")
  );
  let inspect = Inspect_source.inspect_source cmdline.root_choice g in

  let mpstats = get_mpstats g in
  check_guest_free_space inspect mpstats;

  (* Conversion. *)
  let guestcaps =
    let rcaps =
      match conversion_mode with
      | Copying _ ->
         { rcaps_block_bus = None; rcaps_net_bus = None; rcaps_video = None }
      | In_place ->
         rcaps_from_source source in

    do_convert g source inspect
      output#keep_serial_console rcaps cmdline.static_ips in

  g#umount_all ();

  if cmdline.do_copy then (
    (* Doing fstrim on all the filesystems reduces the transfer size
     * because unused blocks are marked in the overlay and thus do
     * not have to be copied.
     *)
    message (f_"Mapping filesystem data to avoid copying unused and blank areas");
    do_fstrim g inspect;
  );

  (match conversion_mode with
   | Copying _ -> message (f_"Closing the overlay")
   | In_place -> message (f_"Closing the source VM")
  );
  g#umount_all ();
  g#shutdown ();
  g#close ();

  (* Copy overlays to target (for [--in-place] this does nothing). *)
  (match conversion_mode with
   | In_place -> ()
   | Copying overlays ->
      message (f_"Assigning disks to buses");
      let target_buses =
        Target_bus_assignment.target_bus_assignment
          source.s_disks source.s_removables guestcaps in
      debug "%s" (string_of_target_buses target_buses);

      let target_firmware =
        get_target_firmware inspect guestcaps source output in

      message (f_"Initializing the target %s") output#as_options;
      let targets =
        (* Decide the format for each output disk. *)
        let target_formats = get_target_formats cmdline output overlays in
        let target_files =
          output#prepare_targets output_name
            (List.combine target_formats overlays)
            guestcaps in
        List.map (
          fun (target_file, target_format, target_overlay) ->
            { target_file; target_format; target_overlay }
        ) (List.combine3 target_files target_formats overlays) in

      (* Perform the copy. *)
      if cmdline.do_copy then
        copy_targets cmdline targets input output;

      (* Create output metadata. *)
      message (f_"Creating output metadata");
      let target_meta = { guestcaps; output_name;
                          target_buses; target_firmware; target_nics } in
      output#create_metadata source inspect target_meta targets;

      delete_target_on_exit := false  (* Don't delete target on exit. *)
  );
  message (f_"Finishing off")

and open_source cmdline input =
  message (f_"Opening the source %s") input#as_options;
  let bandwidth = cmdline.bandwidth in
  let source = input#source ?bandwidth () in

  (* Print source and stop. *)
  if cmdline.print_source then (
    printf (f_"Source guest information (--print-source option):\n");
    printf "\n";
    printf "%s\n" (string_of_source source);
    exit 0
  );

  debug "%s" (string_of_source source);

  (match source.s_hypervisor with
  | OtherHV hv ->
    warning (f_"unknown source hypervisor (‘%s’) in metadata") hv
  | _ -> ()
  );

  assert (source.s_name <> "");
  assert (source.s_memory > 0L);

  assert (source.s_vcpu >= 1);
  assert (source.s_cpu_vendor <> Some "");
  assert (source.s_cpu_model <> Some "");
  (match source.s_cpu_topology with
   | None -> () (* no topology specified *)
   | Some { s_cpu_sockets = sockets; s_cpu_cores = cores;
            s_cpu_threads = threads } ->
      assert (sockets > 0);
      assert (cores > 0);
      assert (threads > 0);
      let expected_vcpu = sockets * cores * threads in
      if expected_vcpu <> source.s_vcpu then
        warning (f_"source sockets * cores * threads <> number of vCPUs.\nSockets %d * cores per socket %d * threads %d = %d, but number of vCPUs = %d.\n\nThis is a problem with either the source metadata or the virt-v2v input module.  In some circumstances this could stop the guest from booting on the target.")
                sockets cores threads expected_vcpu source.s_vcpu
  );

  if source.s_disks = [] then
    error (f_"source has no hard disks!");
  let () =
    let ids = ref IntSet.empty in
    List.iter (
      fun { s_qemu_uri; s_disk_id } ->
        assert (s_qemu_uri <> "");
        (* Check s_disk_id are all unique. *)
        assert (not (IntSet.mem s_disk_id !ids));
        ids := IntSet.add s_disk_id !ids
    ) source.s_disks in

  source

(* Conversion can fail or hang if there is insufficient free space in
 * the temporary directory used to store overlays on the host
 * (RHBZ#1316479).  Although only a few hundred MB is actually
 * required, make the minimum be 1 GB to allow for the possible 500 MB
 * guestfs appliance which is also stored here.
 *)
and check_host_free_space () =
  let free_space = StatVFS.free_space (StatVFS.statvfs large_tmpdir) in
  debug "check_host_free_space: large_tmpdir=%s free_space=%Ld"
        large_tmpdir free_space;
  if free_space < 1_073_741_824L then
    error (f_"insufficient free space in the conversion server temporary directory %s (%s).\n\nEither free up space in that directory, or set the LIBGUESTFS_CACHEDIR environment variable to point to another directory with more than 1GB of free space.\n\nSee also the virt-v2v(1) manual, section \"Minimum free space check in the host\".")
          large_tmpdir (human_size free_space)

(* Create a qcow2 v3 overlay to protect the source image(s). *)
and create_overlays source_disks =
  message (f_"Creating an overlay to protect the source from being modified");
  List.mapi (
    fun i ({ s_qemu_uri = qemu_uri; s_format = format } as source) ->
      let overlay_file =
        Filename.temp_file ~temp_dir:large_tmpdir "v2vovl" ".qcow2" in
      unlink_on_exit overlay_file;

      (* There is a specific reason to use the newer qcow2 variant:
       * Because the L2 table can store zero clusters efficiently, and
       * because discarded blocks are stored as zero clusters, this
       * should allow us to fstrim/blkdiscard and avoid copying
       * significant parts of the data over the wire.
       *)
      let options =
        "compat=1.1" ^
          (match format with None -> ""
                           | Some fmt -> ",backing_fmt=" ^ fmt) in
      let cmd = [ "qemu-img"; "create"; "-q"; "-f"; "qcow2"; "-b"; qemu_uri;
                  "-o"; options; overlay_file ] in
      if run_command cmd <> 0 then
        error (f_"qemu-img command failed, see earlier errors");

      (* Sanity check created overlay (see below). *)
      if not ((open_guestfs ())#disk_has_backing_file overlay_file) then
        error (f_"internal error: qemu-img did not create overlay with backing file");

      let sd = "sd" ^ drive_name i in

      let vsize = (open_guestfs ())#disk_virtual_size overlay_file in

      (* If the virtual size is 0, then something went badly wrong.
       * It could be RHBZ#1283588 or some other problem with qemu.
       *)
      if vsize = 0L then
        error (f_"guest disk %s appears to be zero bytes in size.\n\nThere could be several reasons for this:\n\nCheck that the guest doesn't really have a zero-sized disk.  virt-v2v cannot convert such a guest.\n\nIf you are converting a guest from an ssh source and the guest has a disk on a block device (eg. on a host partition or host LVM LV), then conversions of this type are not supported.  See the virt-v2v-input-xen(1) manual for a workaround.")
              sd;

      (* Function 'actual_target_size' may replace the
       * ov_stats.target_actual_size field.
       *)
      { ov_overlay_file = overlay_file; ov_sd = sd;
        ov_virtual_size = vsize; ov_source = source;
        ov_stats = { target_actual_size = None }
      }
  ) source_disks

(* Populate guestfs handle with qcow2 overlays. *)
and populate_overlays g overlays =
  List.iter (
    fun ({ov_overlay_file = overlay_file}) ->
      g#add_drive_opts overlay_file
        ~format:"qcow2" ~cachemode:"unsafe" ~discard:"besteffort"
        ~copyonread:true
  ) overlays

(* Populate guestfs handle with source disks.  Only used for [--in-place]. *)
and populate_disks g source_disks =
  List.iter (
    fun ({s_qemu_uri = qemu_uri; s_format = format}) ->
      g#add_drive_opts qemu_uri ?format ~cachemode:"unsafe"
                          ~discard:"besteffort"
  ) source_disks

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
    List.iter (print_mpstat Pervasives.stderr) mpstats
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
and do_convert g source inspect keep_serial_console rcaps interfaces =
  (match inspect.i_product_name with
  | "unknown" ->
    message (f_"Converting the guest to run on KVM")
  | prod ->
    message (f_"Converting %s to run on KVM") prod
  );

  let conversion_name, convert =
    try Modules_list.find_convert_module inspect
    with Not_found ->
      error (f_"virt-v2v is unable to convert this guest type (%s/%s)")
        inspect.i_type inspect.i_distro in
  debug "picked conversion module %s" conversion_name;
  debug "requested caps: %s" (string_of_requested_guestcaps rcaps);
  let guestcaps =
    convert g source inspect keep_serial_console rcaps interfaces in
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

(* Decide the format for each output disk.  Output modes can
 * override this, followed by command line -of option, followed
 * by source disk format.
 *)
and get_target_formats cmdline output overlays =
  List.map (
    fun ov ->
      let format =
        match output#override_output_format ov with
        | Some format -> format
        | None ->
           match cmdline.output_format with
           | Some format -> format
           | None ->
              match ov.ov_source.s_format with
              | Some format -> format
              | None ->
                 error (f_"disk %s (%s) has no defined format.\n\nThe input metadata did not define the disk format (eg. raw/qcow2/etc) of this disk, and so virt-v2v will try to autodetect the format when reading it.\n\nHowever because the input format was not defined, we do not know what output format you want to use.  You have two choices: either define the original format in the source metadata, or use the ‘-of’ option to force the output format.") ov.ov_sd ov.ov_source.s_qemu_uri in

      (* What really happens here is that the call to #disk_create
       * below fails if the format is not raw or qcow2.  We would
       * have to extend libguestfs to support further formats, which
       * is trivial, but we'd want to check that the files being
       * created by qemu-img really work.  In any case, fail here,
       * early, not below, later.
       *)
      if format <> "raw" && format <> "qcow2" then
        error (f_"output format should be ‘raw’ or ‘qcow2’.\n\nUse the ‘-of <format>’ option to select a different output format for the converted guest.\n\nOther output formats are not supported at the moment, although might be considered in future.");

      (* Only allow compressed with qcow2. *)
      if cmdline.compressed && format <> "qcow2" then
        error (f_"the --compressed flag is only allowed when the output format is qcow2 (-of qcow2)");

      format
  ) overlays

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

and delete_target_on_exit = ref true

(* Copy the source (really, the overlays) to the output. *)
and copy_targets cmdline targets input output =
  at_exit (fun () ->
    if !delete_target_on_exit then (
      List.iter (
        fun t ->
          match t.target_file with
          | TargetURI _ -> ()
          | TargetFile filename ->
             if not (is_block_device filename) then (
               try unlink filename with _ -> ()
             )
      ) targets
    )
  );
  let nr_disks = List.length targets in
  List.iteri (
    fun i t ->
      (match t.target_file with
       | TargetFile s ->
          message (f_"Copying disk %d/%d to %s (%s)")
                  (i+1) nr_disks s t.target_format;
       | TargetURI s ->
          message (f_"Copying disk %d/%d to qemu URI %s (%s)")
                  (i+1) nr_disks s t.target_format
      );
      debug "%s" (string_of_overlay t.target_overlay);
      debug "%s" (string_of_target t);

      (* We noticed that qemu sometimes corrupts the qcow2 file on
       * exit.  This only seemed to happen with lazy_refcounts was
       * used.  The symptom was that the header wasn't written back
       * to the disk correctly and the file appeared to have no
       * backing file.  Just sanity check this here.
       *)
      let overlay_file = t.target_overlay.ov_overlay_file in
      if not ((open_guestfs ())#disk_has_backing_file overlay_file) then
        error (f_"internal error: qemu corrupted the overlay file");

      (match t.target_file with
       | TargetFile filename ->
          (* As a special case, allow output to a block device or
           * symlink to a block device.  In this case we don't
           * create/overwrite the block device.  (RHBZ#1868690).
           *)
          if not (is_block_device filename) then (
            (* It turns out that libguestfs's disk creation code is
             * considerably more flexible and easier to use than
             * qemu-img, so create the disk explicitly using libguestfs
             * then pass the 'qemu-img convert -n' option so qemu reuses
             * the disk.
             *
             * Also we allow the output mode to actually create the disk
             * image.  This lets the output mode set ownership and
             * permissions correctly if required.
             *)
            (* What output preallocation mode should we use? *)
            let preallocation =
              match t.target_format, cmdline.output_alloc with
              | ("raw"|"qcow2"), Sparse -> Some "sparse"
              | ("raw"|"qcow2"), Preallocated -> Some "full"
              | _ -> None (* ignore -oa flag for other formats *) in
            let compat =
              match t.target_format with "qcow2" -> Some "1.1" | _ -> None in
            output#disk_create filename t.target_format
                               t.target_overlay.ov_virtual_size
                               ?preallocation ?compat
          )

       | TargetURI _ ->
          (* XXX For the moment we assume that qemu URI outputs
           * need no special work.  We can change this in future.
           *)
          ()
      );

      let cmd =
        let filename =
          match t.target_file with
          | TargetFile filename -> qemu_input_filename filename
          | TargetURI uri -> uri in
        [ "qemu-img"; "convert" ] @
        (if not (quiet ()) then [ "-p" ] else []) @
        [ "-n"; "-f"; "qcow2"; "-O"; output#transfer_format t ] @
        (if cmdline.compressed then [ "-c" ] else []) @
        [ "-S"; "64k" ] @
        (if output#write_out_of_order then [ "-W" ] else []) @
        [ overlay_file; filename ] in
      let start_time = gettimeofday () in
      if run_command cmd <> 0 then
        error (f_"qemu-img command failed, see earlier errors");
      let end_time = gettimeofday () in

      (* Calculate the actual size on the target. *)
      actual_target_size t.target_file t.target_overlay.ov_stats;

      (* If verbose, print the virtual and real copying rates. *)
      let elapsed_time = end_time -. start_time in
      if verbose () && elapsed_time > 0. then (
        let mbps size time =
          Int64.to_float size /. 1024. /. 1024. *. 10. /. time
        in

        eprintf "virtual copying rate: %.1f M bits/sec\n%!"
          (mbps t.target_overlay.ov_virtual_size elapsed_time);

        match t.target_overlay.ov_stats.target_actual_size with
        | None -> ()
        | Some actual ->
           eprintf "real copying rate: %.1f M bits/sec\n%!"
                   (mbps actual elapsed_time)
      );

      (* Let the output mode know that the disk was copied successfully,
       * so it can perform any operations without waiting for all the
       * other disks to be copied (i.e. before the metadata is actually
       * created).
       *)
      output#disk_copied t i nr_disks
  ) targets

(* Update the target_actual_size field in the target structure. *)
and actual_target_size target_file disk_stats =
  match target_file with
  | TargetFile filename ->
     let size =
       (* Ignore errors because we want to avoid failures after copying. *)
       try Some (du filename)
       with Failure _ | Invalid_argument _ -> None in
     disk_stats.target_actual_size <- size
  | TargetURI _ -> ()

(* Request guest caps based on source configuration. *)
and rcaps_from_source source =
  let source_block_types =
    List.map (fun sd -> sd.s_controller) source.s_disks in
  let source_block_type =
    match List.sort_uniq source_block_types with
    | [] -> error (f_"source has no hard disks!")
    | [t] -> t
    | _ -> error (f_"source has multiple hard disk types!") in
  let block_type =
    match source_block_type with
    | Some Source_virtio_blk -> Some Virtio_blk
    | Some Source_virtio_SCSI -> Some Virtio_SCSI
    | Some (Source_IDE | Source_SATA) -> Some IDE
    | Some t -> error (f_"source has unsupported hard disk type ‘%s’")
                      (string_of_controller t)
    | None -> error (f_"source has unrecognized hard disk type") in

  let source_net_types =
      List.map (fun nic -> nic.s_nic_model) source.s_nics in
  let source_net_type =
    match List.sort_uniq source_net_types with
    | [] -> None
    | [t] -> t
    | _ -> error (f_"source has multiple network adapter model!") in
  let net_type =
    match source_net_type with
    | Some Source_virtio_net -> Some Virtio_net
    | Some Source_e1000 -> Some E1000
    | Some Source_rtl8139 -> Some RTL8139
    | Some t -> error (f_"source has unsupported network adapter model ‘%s’")
                      (string_of_nic_model t)
    | None -> None in

  let video =
    match source.s_video with
    | Some Source_QXL -> Some QXL
    | Some Source_Cirrus -> Some Cirrus
    | Some t -> error (f_"source has unsupported video adapter model ‘%s’")
                      (string_of_source_video t)
    | None -> None in

  {
    rcaps_block_bus = block_type;
    rcaps_net_bus = net_type;
    rcaps_video = video;
  }

let () = run_main_and_handle_errors main
