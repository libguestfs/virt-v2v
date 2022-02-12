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

open C_utils
open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Types
open Utils

open Output

module Json = struct
  type poptions = string * Types.output_allocation * string * string * string

  type t = unit

  let to_string options =
    "-o json" ^
      match options.output_storage with
      | Some os -> " -os " ^ os
      | None -> ""

  let query_output_options () =
    printf (f_"Output options (-oo) which can be used with -o json:

  -oo json-disks-pattern=PATTERN   Pattern for the disks.
")

  let parse_options options source =
    if options.output_conn <> None then
      error_option_cannot_be_used_in_output_mode "json" "-oc";
    if options.output_password <> None then
      error_option_cannot_be_used_in_output_mode "json" "-op";

    let known_pattern_variables = ["DiskNo"; "DiskDeviceName"; "GuestName"] in
    let json_disks_pattern = ref None in
    List.iter (
      fun (k, v) ->
        match k with
        | "json-disks-pattern" ->
           if !json_disks_pattern <> None then
             error (f_"-o json: -oo json-disks-pattern set more than once");
           let vars =
             try Var_expander.scan_variables v
             with Var_expander.Invalid_variable var ->
               error (f_"-o json: -oo json-disks-pattern: invalid variable %%{%s}")
                 var in
           List.iter (
             fun var ->
               if not (List.mem var known_pattern_variables) then
                 error (f_"-o json: -oo json-disks-pattern: unhandled variable %%{%s}")
                   var
           ) vars;
           json_disks_pattern := Some v
        | k ->
           error (f_"-o json: unknown output option ‘-oo %s’") k
      ) options.output_options;

    let json_disks_pattern =
      Option.default "%{GuestName}-%{DiskDeviceName}" !json_disks_pattern in

    (* -os must be set to a directory. *)
    let output_storage =
      match options.output_storage with
      | None ->
         error (f_"-o json: output directory was not specified, use '-os /dir'")
      | Some d when not (is_directory d) ->
         error (f_"-os %s: output directory does not exist or is not a directory") d
      | Some d -> d in

    let output_name = Option.default source.s_name options.output_name in

    (json_disks_pattern,
     options.output_alloc, options.output_format, output_name, output_storage)

  let rec setup dir options source =
    let disks = get_disks dir in
    let json_disks_pattern,
        output_alloc, output_format, output_name,
        output_storage = options in

    List.iter (
      fun (i, size) ->
        let socket = sprintf "%s/out%d" dir i in
        On_exit.unlink socket;

        (* Create the actual output disk. *)
        let outdisk = json_path output_storage output_name
                        json_disks_pattern i in
        mkdir_p (Filename.dirname outdisk) 0o755;

        output_to_local_file output_alloc output_format outdisk size socket
    ) disks

  (* For -o json, return the output disk name of the i'th disk. *)
  and json_path os output_name json_disks_pattern i =
    let outname =
      let vars_fn = function
        | "DiskNo" -> Some (string_of_int (i+1))
        | "DiskDeviceName" -> Some (sprintf "sd%s" (drive_name i))
        | "GuestName" -> Some output_name
        | _ -> assert false
      in
      Var_expander.replace_fn json_disks_pattern vars_fn in
    let outdisk = os // outname in
    let outdisk = absolute_path outdisk in
    outdisk

  let finalize dir options () source inspect target_meta =
    let json_disks_pattern,
        output_alloc, output_format, output_name, output_storage = options in

    let doc =
      Create_json.create_json_metadata source inspect target_meta
        (json_path output_storage output_name json_disks_pattern)
        output_format output_name in
    let doc_string = JSON.string_of_doc ~fmt:JSON.Indented doc in

    if verbose () then (
      eprintf "resulting JSON:\n";
      output_string Stdlib.stderr doc_string;
      eprintf "\n\n%!";
    );

    let file = output_storage // output_name ^ ".json" in
    with_open_out file (
      fun chan ->
        output_string chan doc_string;
        output_char chan '\n'
    )

  let request_size = None
end
