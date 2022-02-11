(* virt-v2v
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

open Types
open Utils

open Output

module RHV = struct
  type t = string * string * string * string list * string list * int64 list

  let to_string options = "-o rhv"

  let query_output_options () =
    printf (f_"No output options can be used in this mode.\n")

  let parse_options options =
    if options.output_password <> None then
      error_option_cannot_be_used_in_output_mode "rhv" "-op";

    (* -os must be set, but at this point we cannot check it. *)
    let output_storage =
      match options.output_storage with
      | None -> error (f_"-o rhv: -os option was not specified")
      | Some d -> d in

    (options.output_alloc, options.output_format, output_storage)

  let rec setup_servers dir disks output_name
                        (output_alloc, output_format, output_storage) =
    (* UID:GID required for files and directories when writing to ESD. *)
    let uid = 36 and gid = 36 in

    (* Create a UID-switching handle.  If we're not root, create a dummy
     * one because we cannot switch UIDs.
     *)
    let running_as_root = geteuid () = 0 in
    let changeuid_t =
      if running_as_root then
        Changeuid.create ~uid ~gid ()
      else
        Changeuid.create () in

    let esd_mp, esd_uuid =
      mount_and_check_storage_domain (s_"Export Storage Domain")
        output_storage in
    debug "RHV: ESD mountpoint: %s\nRHV: ESD UUID: %s" esd_mp esd_uuid;

    (* See if we can write files as UID:GID 36:36. *)
    let () =
      let testfile = esd_mp // esd_uuid // String.random8 () in
      Changeuid.make_file changeuid_t testfile "";
      let stat = stat testfile in
      Changeuid.unlink changeuid_t testfile;
      let actual_uid = stat.st_uid and actual_gid = stat.st_gid in
      debug "RHV: actual UID:GID of new files is %d:%d" actual_uid actual_gid;
      if uid <> actual_uid || gid <> actual_gid then (
        if running_as_root then
          warning (f_"cannot write files to the NFS server as %d:%d, even though we appear to be running as root. This probably means the NFS client or idmapd is not configured properly.\n\nYou will have to chown the files that virt-v2v creates after the run, otherwise RHV-M will not be able to import the VM.") uid gid
        else
          warning (f_"cannot write files to the NFS server as %d:%d. You might want to stop virt-v2v (^C) and rerun it as root.") uid gid
      ) in

    (* Create unique UUIDs for everything *)
    let vm_uuid = uuidgen () in
    (* Generate random image and volume UUIDs for each target disk. *)
    let image_uuids = List.map (fun _ -> uuidgen ()) disks in
    let vol_uuids = List.map (fun _ -> uuidgen ()) disks in

    (* We need to create the target image director(ies) so there's a place
     * for the main program to copy the images to.  However if image
     * conversion fails for any reason then we delete this directory.
     *)
    let images_dir = esd_mp // esd_uuid // "images" in
    List.iter (
      fun image_uuid ->
        let d = images_dir // image_uuid in
        Changeuid.mkdir changeuid_t d 0o755
    ) image_uuids;
    On_exit.f (
      fun () ->
        (* virt-v2v writes v2vdir/done on success only. *)
        let success = Sys.file_exists (dir // "done") in
        if not success then (
          List.iter (
            fun image_uuid ->
              let d = images_dir // image_uuid in
              let cmd = sprintf "rm -rf %s" (quote d) in
              Changeuid.command changeuid_t cmd
            ) image_uuids
        )
    );

    (* The final directory structure should look like this:
     *   /<MP>/<ESD_UUID>/images/
     *      <IMAGE_UUID_1>/<VOL_UUID_1>        # first disk
     *      <IMAGE_UUID_1>/<VOL_UUID_1>.meta   # first disk
     *      <IMAGE_UUID_2>/<VOL_UUID_2>        # second disk
     *      <IMAGE_UUID_2>/<VOL_UUID_2>.meta   # second disk
     *      <IMAGE_UUID_3>/<VOL_UUID_3>        # etc
     *      <IMAGE_UUID_3>/<VOL_UUID_3>.meta   #
     *)

    (* Generate the randomly named target files (just the names).
     * The main code is what generates the files themselves.
     *)
    let filenames =
      List.map (
        fun (image_uuid, vol_uuid) ->
          let filename = images_dir // image_uuid // vol_uuid in
          debug "RHV: disk: %s" filename;
          filename
      ) (List.combine image_uuids vol_uuids) in

    (* Generate the .meta file associated with each volume. *)
    let sizes = List.map snd disks in
    let metas =
      Create_ovf.create_meta_files output_alloc output_format
        esd_uuid image_uuids sizes in
    List.iter (
      fun (filename, meta) ->
        let meta_filename = filename ^ ".meta" in
        Changeuid.make_file changeuid_t meta_filename meta
    ) (List.combine filenames metas);

    (* Set up the NBD servers. *)
    List.iter (
      fun ((i, size), filename) ->
        let socket = sprintf "%s/out%d" dir i in
        On_exit.unlink socket;

        (* Create the actual output disk. *)
        let changeuid f =
          Changeuid.func changeuid_t (
            fun () ->
              (* Run the command to create the file. *)
              f ();
              (* Make the file sufficiently writable so that possibly root, or
               * root squashed nbdkit will definitely be able to open it.
               * An example of how root squashing nonsense makes everyone
               * less secure.
               *)
              chmod filename 0o666
          )
        in
        output_to_local_file ~changeuid
          output_alloc output_format filename size socket
    ) (List.combine disks filenames);

    (* Save parameters since we need them during finalization. *)
    let t = esd_mp, esd_uuid, vm_uuid, image_uuids, vol_uuids, sizes in
    t

  and mount_and_check_storage_domain domain_class os =
    (* The user can either specify -os nfs:/export, or a local directory
     * which is assumed to be the already-mounted NFS export.
     *)
    match String.split ":/" os with
    | mp, "" ->                         (* Already mounted directory. *)
       check_storage_domain domain_class os mp
    | server, export ->
       let export = "/" ^ export in

       (* Create a mountpoint.  Default mode is too restrictive for us
        * when we need to write into the directory as 36:36.
        *)
       let mp = Mkdtemp.temp_dir "v2v." in
       chmod mp 0o755;

       (* Try mounting it. *)
       let cmd = [ "mount"; sprintf "%s:%s" server export; mp ] in
       if run_command cmd <> 0 then
         error (f_"mount command failed, see earlier errors.\n\nThis probably means you didn't specify the right %s path [-os %s], or else you need to rerun virt-v2v as root.") domain_class os;

       (* Make sure it is unmounted at exit. *)
       On_exit.f (
         fun () ->
           let cmd = [ "umount"; mp ] in
           ignore (run_command cmd);
           try rmdir mp with _ -> ()
       );

       check_storage_domain domain_class os mp

  and check_storage_domain domain_class os mp =
    (* Typical SD mountpoint looks like this:
     * $ ls /tmp/mnt
     * 39b6af0e-1d64-40c2-97e4-4f094f1919c7  __DIRECT_IO_TEST__  lost+found
     * $ ls /tmp/mnt/39b6af0e-1d64-40c2-97e4-4f094f1919c7
     * dom_md  images  master
     * We expect exactly one of those magic UUIDs.
     *)
    let entries =
      try Sys.readdir mp
      with Sys_error msg ->
        error (f_"could not read the %s specified by the '-os %s' parameter on the command line.  Is it really an OVirt or RHV-M %s?  The original error is: %s") domain_class os domain_class msg in
    let entries = Array.to_list entries in
    let uuids = List.filter (
        fun entry ->
          String.length entry = 36 &&
          entry.[8] = '-' && entry.[13] = '-' && entry.[18] = '-' &&
          entry.[23] = '-'
      ) entries in
    let uuid =
      match uuids with
      | [uuid] -> uuid
      | [] ->
         error (f_"there are no UUIDs in the %s (%s).  Is it really an OVirt or RHV-M %s?") domain_class os domain_class
      | _::_ ->
         error (f_"there are multiple UUIDs in the %s (%s).  This is unexpected, and may be a bug in virt-v2v or OVirt.") domain_class os in

    (* Check that the domain has been attached to a Data Center by
     * checking that the master/vms directory exists.
     *)
    let () =
      let master_vms_dir = mp // uuid // "master" // "vms" in
      if not (is_directory master_vms_dir) then
        error (f_"%s does not exist or is not a directory.\n\nMost likely cause: Either the %s (%s) has not been attached to any Data Center, or the path %s is not an %s at all.\n\nYou have to attach the %s to a Data Center using the RHV-M / OVirt user interface first.\n\nIf you don’t know what the %s mount point should be then you can also find this out through the RHV-M user interface.")
          master_vms_dir domain_class os os
          domain_class domain_class domain_class in

    (* Looks good, so return the SD mountpoint and UUID. *)
    (mp, uuid)

  let do_finalize dir source inspect target_meta
                  (output_alloc, output_format, output_storage)
                  (esd_mp, esd_uuid, vm_uuid, image_uuids, vol_uuids, sizes) =
    (* UID:GID required for files and directories when writing to ESD. *)
    let uid = 36 and gid = 36 in

    (* Create a UID-switching handle.  If we're not root, create a dummy
     * one because we cannot switch UIDs.
     *)
    let running_as_root = geteuid () = 0 in
    let changeuid_t =
      if running_as_root then
        Changeuid.create ~uid ~gid ()
      else
        Changeuid.create () in

    (* Create the metadata. *)
    let ovf =
      Create_ovf.create_ovf source inspect target_meta sizes
        output_alloc output_format esd_uuid image_uuids vol_uuids
        ~need_actual_sizes:true dir vm_uuid
        Create_ovf.RHVExportStorageDomain in

    (* Write it to the metadata file. *)
    let dir = esd_mp // esd_uuid // "master" // "vms" // vm_uuid in
    Changeuid.mkdir changeuid_t dir 0o755;
    let file = dir // vm_uuid ^ ".ovf" in
    Changeuid.output changeuid_t file (fun chan -> DOM.doc_to_chan chan ovf)

  let setup dir options source =
    if options.output_options <> [] then
      error (f_"no -oo (output options) are allowed here");
    let data = parse_options options in
    let output_name = Option.default source.s_name options.output_name in
    let disks = get_disks dir in
    setup_servers dir disks output_name data

  let finalize dir options source inspect target_meta t =
    let data = parse_options options in
    do_finalize dir source inspect target_meta data t
end
