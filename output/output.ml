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

type options = {
  output_alloc : Types.output_allocation;
  output_conn : string option;
  output_format : string;
  output_options : (string * string) list;
  output_name : string option;
  output_password : string option;
  output_storage : string option;
}

module type OUTPUT = sig
  type poptions
  type t
  val to_string : options -> string
  val query_output_options : unit -> unit
  val parse_options : options -> Types.source -> poptions
  val setup : string -> poptions -> Types.source -> t
  val finalize : string -> poptions -> t ->
                 Types.source -> Types.inspect -> Types.target_meta ->
                 unit
  val request_size : int option
end

let error_option_cannot_be_used_in_output_mode mode opt =
  error (f_"-o %s: %s option cannot be used in this output mode") mode opt

let get_disks dir =
  let rec loop acc i =
    let socket = sprintf "%s/in%d" dir i in
    if Sys.file_exists socket then (
      let size = Utils.with_nbd_connect_unix ~socket NBD.get_size in
      loop ((i, size) :: acc) (i+1)
    )
    else
      List.rev acc
  in
  loop [] 0

let error_if_disk_count_gt dir n =
  let socket = sprintf "%s/in%d" dir n in
  if Sys.file_exists socket then
    error (f_"this output module doesn't support copying more than %d disks") n

type on_exit_kill = Kill | KillAndWait

let output_to_local_file ?(changeuid = fun f -> f ()) ?(compressed = false)
      ?(on_exit_kill = Kill)
      output_alloc output_format filename size socket =
  (* Check nbdkit is installed and has the required plugin. *)
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working.  It is required \
              to use ‘-o disk’.");
  if not (Nbdkit.probe_plugin "file") then
    error (f_"nbdkit-file-plugin is not installed or not working");

  if compressed then (
    (* Only allow compressed with -of qcow2. *)
    if output_format <> "qcow2" then
      error (f_"‘-oo compressed’ is only allowed when the output format \
                is a local qcow2-format file, i.e. ‘-of qcow2’");
  );

  let g = open_guestfs () in
  let preallocation =
    match output_alloc with
    | Preallocated -> Some "full"
    | Sparse -> None in
  changeuid (
    fun () -> g#disk_create ?preallocation filename output_format size
  );

  let pid =
    match output_format with
    | "raw" ->
       let cmd = Nbdkit.create "file" in
       Nbdkit.add_arg cmd "file" filename;
       let _, pid = Nbdkit.run_unix socket cmd in
       pid

    | "qcow2" ->
       let cmd =
         if compressed then (
           let qemu_quote str = String.replace str "," ",," in
           let image_opts = [ "driver=compress";
                              "file.driver=qcow2";
                              "file.file.driver=file";
                              "file.file.filename=" ^ qemu_quote filename ] in
           let image_opts = String.concat "," image_opts in
           let cmd = QemuNBD.create image_opts in
           QemuNBD.set_image_opts cmd true;
           cmd
         )
         else (* not compressed *) (
           let cmd = QemuNBD.create filename in
           QemuNBD.set_format cmd (Some "qcow2");
           cmd
         ) in
       QemuNBD.set_snapshot cmd false;
       let _, pid = QemuNBD.run_unix socket cmd in
       pid

    | _ ->
       error (f_"output mode only supports raw or qcow2 format (format: %s)")
         output_format in

  match on_exit_kill with
  | Kill ->
    (* Kill the NBD server on exit.  (For nbdkit we use --exit-with-parent
     * but it's not supported everywhere).
     *)
    On_exit.kill pid

  | KillAndWait ->
     On_exit.f (
       fun () ->
         kill pid Sys.sigterm;
         (* Errors from the NBD server don't matter.  On successful
          * completion we've already committed the data to disk.
          *)
         ignore (waitpid [] pid)
     )

let disk_path os name i =
  let outdisk = sprintf "%s/%s-sd%s" os name (drive_name i) in
  absolute_path outdisk
