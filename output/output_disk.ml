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

open Create_libvirt_xml
open Output

module Disk = struct
  type poptions = bool * Types.output_allocation * string * string * string

  type t = unit

  let to_string options =
    "-o disk" ^
      match options.output_storage with
      | Some os -> " -os " ^ os
      | None -> ""

  let query_output_options () =
    printf (f_"Output options that can be used with -o disk:

  -oo compressed      Compress the output file (used only with -of qcow2)
")

  let parse_options options source =
    let compressed = ref false in
    List.iter (
      function
      | "compressed", "" -> compressed := true
      | "compressed", v -> compressed := bool_of_string v
      | k, _ ->
         error (f_"-o disk: unknown output option ‘-oo %s’") k
    ) options.output_options;

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

    let output_name = Option.value ~default:source.s_name options.output_name in

    !compressed, options.output_alloc, options.output_format,
    output_name, output_storage

  let setup dir options source input_disks =
    let compressed, output_alloc, output_format, output_name, output_storage =
      options in

    let uris =
      create_local_output_disks dir ~compressed output_alloc output_format
        output_name output_storage input_disks in

    (), uris

  let finalize dir options () output_disks source inspect target_meta =
    let _, output_alloc, output_format, output_name, output_storage = options in

    (* We don't know what target features the hypervisor supports, but
     * assume a common set that libvirt supports.
     *)
    let domcaps_features =
      match target_meta.guestcaps.gcaps_arch with
      | "i686" | "x86_64" -> { supports_floppy = true }
      | _ -> { supports_floppy = false } in

    let target_features =
      match target_meta.guestcaps.gcaps_arch with
      | "i686" -> [ "acpi"; "apic"; "pae" ]
      | "x86_64" -> [ "acpi"; "apic" ]
      | _ -> [] in

    let doc = create_libvirt_xml source inspect target_meta
                target_features domcaps_features
                (disk_path output_storage output_name)
                output_format output_name in

    let file = output_storage // (sanitize_slash output_name) ^ ".xml" in
    with_open_out file (fun chan -> DOM.doc_to_chan chan doc);

    if verbose () then (
      eprintf "resulting local libvirt XML:\n";
      DOM.doc_to_chan Stdlib.stderr doc;
      eprintf "\n%!";
    )

  let request_size = None
end
