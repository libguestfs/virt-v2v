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
open Common_gettext.Gettext

open Types
open Utils

open Create_libvirt_xml
open Output

module Disk = struct
  type t = unit

  let to_string options =
    "-o disk" ^
      match options.output_storage with
      | Some os -> " -os " ^ os
      | None -> ""

  let query_output_options () =
    printf (f_"No output options can be used in this mode.\n")

  let parse_options options =
    if options.output_password <> None then
      error_option_cannot_be_used_in_output_mode "local" "-op";

    (* -os must be set to a directory. *)
    let output_storage =
      match options.output_storage with
      | None ->
         error (f_"-o disk: output directory was not specified, use '-os /dir'")
      | Some d when not (is_directory d) ->
         error (f_"-os %s: output directory does not exist or is not a directory") d
      | Some d -> d in
    options.output_alloc, options.output_format, output_storage

  let setup_servers dir disks output_name
                    (output_alloc, output_format, output_storage) =
    List.iter (
      fun (i, size) ->
        let socket = sprintf "%s/out%d" dir i in
        On_exit.unlink socket;

        (* Create the actual output disk. *)
        let outdisk = disk_path output_storage output_name i in
        output_to_local_file output_alloc output_format outdisk size socket
    ) disks

  let do_finalize dir source inspect target_meta
                  (output_alloc, output_format, output_storage) =
    (* Convert metadata to libvirt XML. *)
    (match target_meta.target_firmware with
     | TargetBIOS -> ()
     | TargetUEFI ->
        (* XXX Can remove this method when libvirt supports
         * <loader type="efi"/> since then it will be up to
         * libvirt to check this.
         *)
        error_unless_uefi_firmware target_meta.guestcaps.gcaps_arch
    );

    (* We don't know what target features the hypervisor supports, but
     * assume a common set that libvirt supports.
     *)
    let target_features =
      match target_meta.guestcaps.gcaps_arch with
      | "i686" -> [ "acpi"; "apic"; "pae" ]
      | "x86_64" -> [ "acpi"; "apic" ]
      | _ -> [] in

    let output_name = target_meta.output_name in

    let doc = create_libvirt_xml source inspect target_meta
                target_features
                (disk_path output_storage output_name)
                output_format in

    let file = output_storage // output_name ^ ".xml" in
    with_open_out file (fun chan -> DOM.doc_to_chan chan doc);

    if verbose () then (
      eprintf "resulting local libvirt XML:\n";
      DOM.doc_to_chan Stdlib.stderr doc;
      eprintf "\n%!";
    )

  let setup dir options source =
    if options.output_options <> [] then
      error (f_"no -oo (output options) are allowed here");
    let data = parse_options options in
    let output_name = Option.default source.s_name options.output_name in
    let disks = get_disks dir in
    setup_servers dir disks output_name data

  let finalize dir options source inspect target_meta () =
    let data = parse_options options in
    do_finalize dir source inspect target_meta data
end
