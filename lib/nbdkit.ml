(* virt-v2v
 * Copyright (C) 2009-2020 Red Hat Inc.
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
  let test = lazy (Sys.command "nbdkit --version >/dev/null 2>&1" = 0) in
  fun () -> Lazy.force test

type config = (string * string) list

let config () =
  let cmd = "nbdkit --dump-config" in
  let lines = external_command cmd in
  List.map (String.split "=") lines

type version = int * int * int

let version =
  let rex = PCRE.compile "^(\\d+)\\.(\\d+)\\.(\\d+)" in
  fun config ->
    let version =
      try List.assoc "version" config
      with Not_found -> failwith "nbdkit: no version in --dump-config" in
    if not (PCRE.matches rex version) then
      error (f_"nbdkit: unexpected version in --dump-config: %s") version;
    let major = int_of_string (PCRE.sub 1)
    and minor = int_of_string (PCRE.sub 2)
    and release = int_of_string (PCRE.sub 3) in
    debug "nbdkit version: %d.%d.%d" major minor release;
    (major, minor, release)

let probe_plugin name =
  let cmd = sprintf "nbdkit %s --version >/dev/null 2>&1" (quote name) in
  Sys.command cmd = 0

let probe_filter name =
  let cmd = sprintf "nbdkit null --filter=%s --version >/dev/null 2>&1"
              (quote name) in
  Sys.command cmd = 0

let probe_filter_parameter name regex =
  let cmd = sprintf "nbdkit null --filter=%s --help | grep -sq %s"
              (quote name) (quote regex) in
  Sys.command cmd = 0

type cmd = {
  plugin : string option;
  filters : string list;
  args : (string * string) list; (* stored reversed *)
  env : (string * string) list;
  debug_flags : (string * string) list; (* stored reversed *)
  readonly : bool;
  threads : int;
  verbose : bool;
}

let new_cmd = {
  plugin = None;
  filters = [];
  args = [];
  env = [ "LANG", "C" ];
  debug_flags = [];
  readonly = false;
  threads = 16;
  verbose = false;
}

let add_debug_flag cmd name value =
  { cmd with debug_flags = (name, value) :: cmd.debug_flags }

let set_readonly cmd v = { cmd with readonly = v }
let set_threads cmd v = { cmd with threads = v }
let set_verbose cmd v = { cmd with verbose = v }
let set_plugin cmd v = { cmd with plugin = Some v }
let add_filter cmd v = { cmd with filters = v :: cmd.filters }
let add_arg cmd key value = { cmd with args = (key, value) :: cmd.args }
let add_args cmd kvs = { cmd with args = List.rev kvs @ cmd.args }
let add_env cmd name value = { cmd with env = (name, value) :: cmd.env }

let add_filter_if_available cmd filter =
  if probe_filter filter then add_filter cmd filter else cmd

let run_unix ?socket cmd =
  (* Create a temporary directory where we place the socket and PID file.
   * Use the libguestfs socket directory, so it is more likely the full path
   * of the UNIX sockets will fit in the (limited) socket pathname.
   *)
  let tmpdir =
    let base_dir = (open_guestfs ())#get_sockdir () in
    let t = Mkdtemp.temp_dir ~base_dir "v2vnbdkit." in
    (* tmpdir must be readable (but not writable) by "other" so that
     * qemu can open the sockets.
     *)
    chmod t 0o755;
    On_exit.rmdir t;
    t in

  let id = unique () in
  let pidfile = tmpdir // sprintf "nbdkit%d.pid" id in
  let socket =
    match socket with
    | None -> tmpdir // sprintf "nbdkit%d.sock" id
    | Some socket -> socket in

  (* Construct the final command line. *)
  let add_arg, add_args_reversed, get_args =
    let r = ref [] in
    let add_arg v = List.push_front v r in
    let add_args_reversed vs = r := vs @ !r in
    let get_args () = List.rev !r in
    add_arg, add_args_reversed, get_args
  in

  add_arg "nbdkit";
  add_arg "--exit-with-parent";
  add_arg "--foreground";
  add_arg "--pidfile"; add_arg pidfile;
  add_arg "--unix"; add_arg socket;
  add_arg "--threads"; add_arg (string_of_int cmd.threads);

  if have_selinux then (
    add_arg "--selinux-label";
    add_arg "system_u:object_r:svirt_socket_t:s0"
  );

  (* Reduce verbosity in nbdkit >= 1.17.4. *)
  let version = version (config ()) in
  if version >= (1, 17, 4) then (
    add_arg "-D"; add_arg "nbdkit.backend.datapath=0"
  );

  List.iter (
    fun (name, value) ->
      add_arg "-D"; add_arg (sprintf "%s=%s" name value)
  ) (List.rev cmd.debug_flags);
  if cmd.readonly then add_arg "--readonly";
  if cmd.verbose then add_arg "--verbose";
  List.iter (fun filter -> add_arg "--filter"; add_arg filter) cmd.filters;

  (match cmd.plugin with
   | None -> assert false
   | Some plugin -> add_arg plugin);
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
    execvp "nbdkit" args
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

  (* Set the regular Unix permissions, in case qemu is
   * running as another user.
   *)
  chmod socket 0o777;

  socket, pid
