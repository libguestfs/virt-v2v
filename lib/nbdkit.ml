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

open Unix
open Printf

open Common_gettext.Gettext
open Std_utils
open Tools_utils
open Unix_utils

open Utils

let is_installed =
  let cmd = sprintf "%s --version >/dev/null 2>&1" Config.nbdkit in
  let test = lazy (Sys.command cmd = 0) in
  fun () -> Lazy.force test

type config = (string * string) list

let config =
  let output =
    lazy (
      let cmd = sprintf "%s --dump-config" Config.nbdkit in
      let lines = external_command cmd in
      List.map (String.split "=") lines
    ) in
  fun () -> Lazy.force output

type version = int * int * int

let rex = PCRE.compile "^(\\d+)\\.(\\d+)\\.(\\d+)"
let version =
  let v =
    lazy (
      let config = config () in
      let version =
        try List.assoc "version" config
        with Not_found -> failwith "nbdkit: no version in --dump-config" in
      if not (PCRE.matches rex version) then
        error (f_"nbdkit: unexpected version in --dump-config: %s") version;
      let major = int_of_string (PCRE.sub 1)
      and minor = int_of_string (PCRE.sub 2)
      and release = int_of_string (PCRE.sub 3) in
      if verbose () then (
        eprintf "info: nbdkit version:\n%!";
        ignore (Sys.command (sprintf "%s --version >&2" Config.nbdkit));
        eprintf "parsed version: %d.%d.%d\n%!" major minor release
      );
      (major, minor, release)
    ) in
  fun () -> Lazy.force v

let probe_server_parameter name =
  let list_options =
    if String.length name = 2 &&
         name.[0] = '-' &&
         name.[1] <> '-' then
      "--short-options"
    else
      "--long-options" in
  let regex = sprintf "^%s$" name in
  let cmd = sprintf "nbdkit %s | grep -sq %s" list_options (quote regex) in
  Sys.command cmd = 0

let probe_plugin name =
  let cmd = sprintf "%s %s --version >/dev/null 2>&1" Config.nbdkit (quote name) in
  Sys.command cmd = 0

let probe_plugin_parameter name regex =
  let cmd = sprintf "%s %s --help | grep -sq %s"
              Config.nbdkit (quote name) (quote regex) in
  Sys.command cmd = 0

let probe_filter name =
  let cmd = sprintf "%s null --filter=%s --version >/dev/null 2>&1"
              Config.nbdkit (quote name) in
  Sys.command cmd = 0

let probe_filter_parameter name regex =
  let cmd = sprintf "%s null --filter=%s --help | grep -sq %s"
              Config.nbdkit (quote name) (quote regex) in
  Sys.command cmd = 0

type cmd = {
  plugin : string;
  name : string option;
  mutable filters : string list;
  mutable args : (string * string) list; (* stored reversed *)
  mutable env : (string * string) list;
  mutable debug_flags : (string * string) list;
  mutable readonly : bool;
  mutable threads : int;
  verbose : bool;
}

let create ?(quiet = false) ?name plugin = {
  plugin;
  name;
  filters = [];
  args = [];
  env = [ "LANG", "C" ];
  debug_flags = [];
  readonly = false;
  threads = 16;
  verbose = not quiet && verbose ()
}

let add_debug_flag cmd name value =
  cmd.debug_flags <- (name, value) :: cmd.debug_flags

let set_readonly cmd v = cmd.readonly <- v
let set_threads cmd v = cmd.threads <- v
let add_filter cmd v = cmd.filters <- v :: cmd.filters
let add_arg cmd key value = cmd.args <- (key, value) :: cmd.args
let add_args cmd kvs = cmd.args <- List.rev kvs @ cmd.args
let add_env cmd name value = cmd.env <- (name, value) :: cmd.env

let add_filter_if_available cmd filter =
  if probe_filter filter then add_filter cmd filter

let run_unix socket cmd =
  (* Create a temporary directory where we place the PID file. *)
  let piddir = Mkdtemp.temp_dir "v2vnbdkit." in
  On_exit.rm_rf piddir;

  let id = unique () in
  let pidfile = piddir // sprintf "nbdkit%d.pid" id in

  (* Construct the final command line. *)
  let add_arg, add_args_reversed, get_args =
    let r = ref [] in
    let add_arg v = List.push_front v r in
    let add_args_reversed vs = r := vs @ !r in
    let get_args () = List.rev !r in
    add_arg, add_args_reversed, get_args
  in

  add_arg Config.nbdkit;

  (match cmd.name with
   | None -> ()
   | Some name ->
      (* --name parameter should be first, so it appears early in 'ps'. *)
      if probe_server_parameter "--name" then (
        add_arg "--name";
        add_arg name
      )
  );

  add_arg "--exit-with-parent";
  add_arg "--foreground";
  add_arg "--pidfile"; add_arg pidfile;
  add_arg "--unix"; add_arg socket;
  add_arg "--threads"; add_arg (string_of_int cmd.threads);

  if have_selinux then (
    add_arg "--selinux-label";
    add_arg "system_u:object_r:svirt_socket_t:s0"
  );

  (* Reduce verbosity in nbdkit. *)
  add_arg "-D"; add_arg "nbdkit.backend.datapath=0";

  List.iter (
    fun (name, value) ->
      add_arg "-D"; add_arg (sprintf "%s=%s" name value)
  ) cmd.debug_flags;
  if cmd.readonly then add_arg "--readonly";
  if cmd.verbose then add_arg "--verbose";
  List.iter (fun filter -> add_arg "--filter"; add_arg filter) cmd.filters;

  add_arg cmd.plugin;
  add_args_reversed (List.map (fun (k, v) -> sprintf "%s=%s" k v) cmd.args);
  let args = get_args () in

  (* Print the full command we are about to run when debugging. *)
  if verbose () then (
    eprintf "running nbdkit:\n";
    List.iter (fun (k, v) -> eprintf " %s=%s" k v) cmd.env;
    List.iter (fun arg -> eprintf " %s" (quote arg)) args;
    prerr_newline ()
  );

  (* Start an nbdkit instance in the background. *)
  let args = Array.of_list args in
  let pid = fork () in
  if pid = 0 then (
    (* Child process (nbdkit). *)
    List.iter (fun (k, v) -> putenv k v) cmd.env;
    execvp Config.nbdkit args
  );

  (* Wait for the pidfile to appear so we know that nbdkit
   * is listening for requests.
   *)
  if not (wait_for_file pidfile 30) then (
    if verbose () then
      error (f_"nbdkit did not start up.  See previous debugging messages for problems.")
    else
      error (f_"nbdkit did not start up.  There may be errors printed by nbdkit above.

If the messages above are not sufficient to diagnose the problem then add the ‘virt-v2v -v -x’ options and examine the debugging output carefully.")
  );

  if have_selinux then (
    (* Note that Unix domain sockets have both a file label and
     * a socket/process label.  Using --selinux-label above
     * only set the socket label, but we must also set the file
     * label.
     *)
    ignore (run_command ["chcon"; "system_u:object_r:svirt_image_t:s0";
                         socket]);
  );

  (* Set the regular Unix permissions, in case nbdkit is
   * running as another user.
   *)
  chown_for_libvirt_rhbz_1045069 socket;
  chmod socket 0o700;

  socket, pid
