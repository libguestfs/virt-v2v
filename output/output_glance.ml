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

module Glance = struct
  type poptions = string * string

  type t = string

  let to_string options = "-o glance"

  let query_output_options () =
    printf (f_"No output options can be used in this mode.\n")

  let parse_options options source =
    if options.output_options <> [] then
      error (f_"no -oo (output options) are allowed here");
    if options.output_conn <> None then
      error_option_cannot_be_used_in_output_mode "glance" "-oc";
    if options.output_password <> None then
      error_option_cannot_be_used_in_output_mode "glance" "-op";
    if options.output_storage <> None then
      error_option_cannot_be_used_in_output_mode "glance" "-os";

    let output_name = Option.value ~default:source.s_name options.output_name in

    options.output_format, output_name

  let setup dir options source =
    let disks = get_disks dir in
    let output_format, output_name = options in

    (* This does nothing useful except to check that the user has
     * supplied all the correct auth environment variables to make
     * 'glance' commands work as the current user.  If not then the
     * program exits early.
     *)
    if shell_command "glance image-list > /dev/null" <> 0 then
      error (f_"glance: glance client is not installed or set up correctly.  \
                You may need to set environment variables or source a script \
                to enable authentication.  \
                See preceding messages for details.");

    (* When debugging, query the glance client for its version. *)
    if verbose () then (
      eprintf "version of the glance client:\n%!";
      ignore (shell_command "glance --version");
    );

    (* Although glance can slurp in a stream from stdin, qemu-nbd
     * (used for -of qcow2) cannot write to a stream.  This might
     * be possible in future with more creative use of NBD.  (XXX)
     *)
    let tmpdir = Mkdtemp.temp_dir ~base_dir:large_tmpdir "glance." in

    (* This will write disks to the large temporary directory. *)
    List.iter (
      fun (i, size) ->
        let socket = sprintf "%s/out%d" dir i in
        On_exit.unlink socket;

        (* Create the actual output disk. *)
        let outdisk = sprintf "%s/%d" tmpdir i in
        output_to_local_file Sparse output_format outdisk size socket
    ) disks;

    tmpdir

  let finalize dir options tmpdir source inspect target_meta =
    let output_format, output_name = options in

    let min_ram = source.s_memory /^ 1024L /^ 1024L in

    (* Get the image properties. *)
    let properties =
      Openstack_image_properties.create source inspect target_meta in
    let properties =
      List.flatten (
          List.map (
              fun (k, v) -> [ "--property"; sprintf "%s=%s" k v ]
            ) properties
        ) in

    (* The first disk, assumed to be the system disk, will be called
     * "guestname".  Subsequent disks, assumed to be data disks,
     * will be called "guestname-disk2" etc.  The manual strongly
     * hints you should import the data disks to Cinder.
     *)
    List.iteri (
      fun i _ ->
        let name =
          if i == 0 then output_name
          else sprintf "%s-disk%d" output_name (i+1) in

        let disk = sprintf "%s/%d" tmpdir i in

        (* If glance is used with VMware then there's a vmware_disktype
         * option which allows preallocated.  However I don't believe
         * it's possible in general glance, so ignore the -oa option.
         * Since we are writing to a temporary file before copying to
         * glance, -oa preallocated just preallocates the temporary file.
         *)
        let cmd = [ "glance"; "image-create"; "--name"; name;
                    "--disk-format=" ^ output_format;
                    "--container-format=bare"; "--file"; disk;
                    "--min-ram"; Int64.to_string min_ram ] @
                    properties in
        if run_command cmd <> 0 then
          error (f_"glance: image upload to glance failed, see earlier errors");

        (* Unlink the temporary files as soon as glance has got them. *)
        try unlink disk with Unix_error _ -> ()
    ) source.s_disks;

    (* Remove the temporary directory for the large files. *)
    (try rmdir tmpdir with Unix_error _ -> ())

  let request_size = None
end
