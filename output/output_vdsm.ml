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
open Unix

open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Types
open Utils

open Output

module VDSM = struct
  type poptions = Types.output_allocation * string * string * string *
                  string list * string list * string * string *
                  string * Create_ovf.ovf_flavour

  type t = string * string * int64 list

  let to_string options = "-o vdsm"

  let query_output_options () =
    let ovf_flavours_str = String.concat "|" Create_ovf.ovf_flavours in

    printf (f_"Output options (-oo) which can be used with -o vdsm:

  -oo vdsm-compat=0.10|1.1     Write qcow2 with compat=0.10|1.1
                                   (default: 0.10)
  -oo vdsm-vm-uuid=UUID        VM UUID (required)
  -oo vdsm-ovf-output=DIR      OVF metadata directory (required)
  -oo vdsm-ovf-flavour=%s
                               Set the type of generated OVF (default: ovirtexp)

For each disk you must supply one of each of these options:

  -oo vdsm-image-uuid=UUID     Image directory UUID
  -oo vdsm-vol-uuid=UUID       Disk volume UUID
") ovf_flavours_str

  let parse_options options source =
    if options.output_password <> None then
      error_option_cannot_be_used_in_output_mode "vdsm" "-op";

    let vm_uuid = ref None in
    let ovf_output = ref None in (* default "." *)
    let compat = ref "0.10" in
    let ovf_flavour = ref Create_ovf.OVirtExportStorageDomain in
    let image_uuids = ref [] in
    let vol_uuids = ref [] in

    List.iter (
      function
      | "vdsm-compat", "0.10" -> compat := "0.10"
      | "vdsm-compat", "1.1" -> compat := "1.1"
      | "vdsm-compat", v ->
         error (f_"-o vdsm: unknown vdsm-compat level ‘%s’") v
      | "vdsm-vm-uuid", v ->
         if !vm_uuid <> None then
           error (f_"-o vdsm: -oo vdsm-vm-uuid set more than once");
         vm_uuid := Some v;
      | "vdsm-ovf-output", v ->
         if !ovf_output <> None then
           error (f_"-o vdsm: -oo vdsm-ovf-output set more than once");
         ovf_output := Some v;
      | "vdsm-ovf-flavour", v ->
         ovf_flavour := Create_ovf.ovf_flavour_of_string v
      | "vdsm-image-uuid", v ->
         List.push_front v image_uuids
      | "vdsm-vol-uuid", v ->
         List.push_front v vol_uuids
      | k, _ ->
         error (f_"-o vdsm: unknown output option ‘-oo %s’") k
    ) options.output_options;

    let compat = !compat in
    let image_uuids = List.rev !image_uuids in
    let vol_uuids = List.rev !vol_uuids in
    if image_uuids = [] || vol_uuids = [] then
      error (f_"-o vdsm: either -oo vdsm-vol-uuid or \
                -oo vdsm-vm-uuid was not specified");
    let vm_uuid =
      match !vm_uuid with
      | None ->
         error (f_"-o vdsm: -oo vdsm-image-uuid was not specified")
      | Some uuid -> uuid in
    let ovf_output = Option.value ~default:"." !ovf_output in
    let ovf_flavour = !ovf_flavour in

    (* -os must be set, but at this point we cannot check it. *)
    let output_storage =
      match options.output_storage with
      | None -> error (f_"-o vdsm: -os option was not specified")
      | Some d when not (is_directory d) ->
         error (f_"-os %s: output directory does not exist \
                   or is not a directory") d
      | Some d -> d in

    let output_name = Option.value ~default:source.s_name options.output_name in

    (options.output_alloc, options.output_format,
     output_name, output_storage,
     image_uuids, vol_uuids, vm_uuid, ovf_output,
     compat, ovf_flavour)

  let setup dir options source input_disks =
    error_if_disk_count_gt input_disks 23;
    let input_sizes = get_disk_sizes input_disks in
    let output_alloc, output_format,
        output_name, output_storage,
        image_uuids, vol_uuids, vm_uuid, ovf_output,
        compat, ovf_flavour = options in

    if List.length image_uuids <> List.length input_disks ||
       List.length vol_uuids <> List.length input_disks then
      error (f_"the number of ‘-oo vdsm-image-uuid’ and ‘-oo vdsm-vol-uuid’ \
                parameters passed on the command line has to match the \
                number of guest disk images (for this guest: %d)")
        (List.length input_disks);

    let dd_mp, dd_uuid =
      let fields =
        String.nsplit "/" output_storage in (* ... "data-center" "UUID" *)
      let fields = List.rev fields in       (* "UUID" "data-center" ... *)
      let fields = List.drop_while ((=) "") fields in
      match fields with
      | uuid :: rest when String.length uuid = 36 ->
         let mp = String.concat "/" (List.rev rest) in
         mp, uuid
      | _ ->
         error (f_"vdsm: invalid -os parameter \
                   does not contain a valid UUID: %s")
           output_storage in

    debug "VDSM: DD mountpoint: %s\nVDSM: DD UUID: %s" dd_mp dd_uuid;

    (* Note that VDSM has to create all these directories. *)
    let images_dir = dd_mp // dd_uuid // "images" in
    List.iter (
      fun image_uuid ->
        let d = images_dir // image_uuid in
        if not (is_directory d) then
          error (f_"image directory (%s) does not exist or is not a directory")
            d
      ) image_uuids;

    (* Note that VDSM has to create this directory too. *)
    if not (is_directory ovf_output) then
      error (f_"OVF (metadata) directory (%s) does not exist or \
                is not a directory")
        ovf_output;

    debug "VDSM: OVF (metadata) directory: %s" ovf_output;

    (* The final directory structure should look like this:
     *   /<MP>/<ESD_UUID>/images/
     *      <IMAGE_UUID_1>/<VOL_UUID_1>        # first disk
     *      <IMAGE_UUID_1>/<VOL_UUID_1>.meta   # first disk
     *      <IMAGE_UUID_2>/<VOL_UUID_2>        # second disk
     *      <IMAGE_UUID_2>/<VOL_UUID_2>.meta   # second disk
     *      <IMAGE_UUID_3>/<VOL_UUID_3>        # etc
     *      <IMAGE_UUID_3>/<VOL_UUID_3>.meta   #
     *)

    (* Create the target filenames. *)
    let filenames =
      List.map (
        fun (image_uuid, vol_uuid) ->
          let filename = images_dir // image_uuid // vol_uuid in
          debug "VDSM: disk: %s" filename;
          filename
      ) (List.combine image_uuids vol_uuids) in

    (* Generate the .meta files associated with each volume. *)
    let metas =
      Create_ovf.create_meta_files output_alloc output_format
        dd_uuid image_uuids input_sizes in
    List.iter (
      fun (filename, meta) ->
        let meta_filename = filename ^ ".meta" in
        with_open_out meta_filename (fun chan -> output_string chan meta)
    ) (List.combine filenames metas);

    (* Set up the NBD servers. *)
    List.iteri (
      fun i (size, filename) ->
        let socket = sprintf "%s/out%d" dir i in
        On_exit.unlink socket;

        (* Create the actual output disk. *)
        output_to_local_file output_alloc output_format filename size socket
    ) (List.combine input_sizes filenames);

    (* Save parameters since we need them during finalization. *)
    let t = dd_mp, dd_uuid, input_sizes in
    t

  let finalize dir options t source inspect target_meta =
    let output_alloc, output_format,
        output_name, output_storage,
        image_uuids, vol_uuids, vm_uuid, ovf_output,
        compat, ovf_flavour = options in
    let dd_mp, dd_uuid, sizes = t in

    (* Create the metadata. *)
    let ovf = Create_ovf.create_ovf source inspect target_meta sizes
                output_alloc output_format output_name dd_uuid
                image_uuids
                vol_uuids
                dir
                vm_uuid
                ovf_flavour in

    (* Write it to the metadata file. *)
    let file = ovf_output // vm_uuid ^ ".ovf" in
    with_open_out file (fun chan -> DOM.doc_to_chan chan ovf)

  let request_size = None
end
