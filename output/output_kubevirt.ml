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
open Create_kubevirt_yaml

(* Valid output names for Kubevirt (RHBZ#2162332). *)
let rfc1123_re =
  PCRE.compile
    "^[a-z0-9]([-a-z0-9]*[a-z0-9])?(\\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*$"

module Kubevirt = struct
  type poptions = bool * output_allocation * string * string * string

  type t = unit

  let to_string options =
    "-o kubevirt" ^
      match options.output_storage with
      | Some os -> " -os " ^ os
      | None -> ""

  let query_output_options () =
    printf (f_"Output options that can be used with -o kubevirt:

  -oo compressed      Compress the output file (used only with -of qcow2)
")


  let parse_options options source =
    let compressed = ref false in
    List.iter (
      function
      | "compressed", "" -> compressed := true
      | "compressed", v -> compressed := bool_of_string v
      | k, _ ->
         error (f_"-o kubevirt: unknown output option ‘-oo %s’") k
    ) options.output_options;

    if options.output_password <> None then
      error_option_cannot_be_used_in_output_mode "kubevirt" "-op";

    (* -os must be set to a directory. *)
    let output_storage =
      match options.output_storage with
      | None ->
         error (f_"-o kubevirt: output directory was not specified, \
                   use '-os /dir'")
      | Some d when not (is_directory d) ->
         error (f_"-os %s: output directory does not exist or \
                   is not a directory") d
      | Some d -> d in

    let output_name = Option.value ~default:source.s_name options.output_name in

    if not (PCRE.matches rfc1123_re output_name) then
      error (f_"-o kubevirt: the guest name must contain only lowercase \
                alphanumeric characters, '-' or '.', and must start and \
                end with an alphanumeric character.  Rerun virt-v2v with \
                the '-on name' option to rename it.");

    !compressed, options.output_alloc, options.output_format,
    output_name, output_storage

  let setup dir options source input_disks =
    let input_sizes = get_disk_sizes input_disks in
    let compressed, output_alloc, output_format, output_name, output_storage =
      options in

    List.iteri (
      fun i size ->
        let socket = sprintf "%s/out%d" dir i in
        On_exit.unlink socket;

        (* Create the actual output disk. *)
        let outdisk = disk_path output_storage output_name i in
        output_to_local_file ~compressed output_alloc output_format
          outdisk size socket
    ) input_sizes

  let finalize dir options () source inspect target_meta =
    let _, output_alloc, output_format, output_name, output_storage = options in

    let doc = create_kubevirt_yaml source inspect target_meta
                (disk_path output_storage output_name)
                output_format output_name in

    let file = output_storage // output_name ^ ".yaml" in
    with_open_out file (fun chan -> YAML.doc_to_chan chan doc);

    if verbose () then (
      eprintf "resulting kubevirt YAML:\n";
      YAML.doc_to_chan Stdlib.stderr doc;
      eprintf "\n%!";
    )

  let request_size = None
end
