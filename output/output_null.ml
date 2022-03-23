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

open Output

module Null = struct
  type poptions = unit
  type t = unit

  let to_string options = "-o null"

  let query_output_options () =
    printf (f_"No output options can be used in this mode.\n")

  let parse_options options source =
    if options.output_options <> [] then
      error (f_"no -oo (output options) are allowed here");
    if options.output_alloc <> Sparse then
      error_option_cannot_be_used_in_output_mode "null" "-oa";
    if options.output_conn <> None then
      error_option_cannot_be_used_in_output_mode "null" "-oc";
    if options.output_format <> "raw" then
      error_option_cannot_be_used_in_output_mode "null" "-of";
    if options.output_password <> None then
      error_option_cannot_be_used_in_output_mode "null" "-op";
    if options.output_storage <> None then
      error_option_cannot_be_used_in_output_mode "null" "-os"

  let setup dir () source =
    let disks = get_disks dir in

    (* Check nbdkit is installed and has the required plugin. *)
    if not (Nbdkit.is_installed ()) then
      error (f_"nbdkit is not installed or not working.  It is required to use ‘-o null’.");
    if not (Nbdkit.probe_plugin "null") then
      error (f_"nbdkit-null-plugin is not installed or not working");

    (* We only need to run one nbdkit instance (even if there is
     * more than one disk) and we can ignore the size of the inputs
     * and set the size of the output to 7E.
     *)
    let socket = sprintf "%s/out0" dir in
    On_exit.unlink socket;

    let () =
      let cmd = Nbdkit.create ~quiet:true "null" in
      Nbdkit.add_arg cmd "size" "7E";
      let _, pid = Nbdkit.run_unix socket cmd in

      (* --exit-with-parent should ensure nbdkit is cleaned
       * up when we exit, but it's not supported everywhere.
       *)
      On_exit.kill pid in

    (* Use hard links to the same socket for the other disks. *)
    List.iter (
      fun (i, _) ->
        if i > 0 then (
          let output = sprintf "%s/out%d" dir i in
          link socket output
        )
    ) disks

  let finalize dir () () source inspect target_meta =
    () (* nothing to do *)

  let request_size = None
end
