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

open Printf

open Common_gettext.Gettext
open Std_utils
open Tools_utils

open Types
open Utils

let nbdkit_min_version = (1, 12, 0)
let nbdkit_min_version_string = "1.12.0"

type password =
| NoPassword                    (* no password option at all *)
| AskForPassword                (* password=- *)
| PasswordFile of string        (* password=+file *)

(* Check that nbdkit is available and new enough. *)
let error_unless_nbdkit_working () =
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working")

let error_unless_nbdkit_min_version config =
  let version = Nbdkit.version config in
  if version < nbdkit_min_version then
    error (f_"nbdkit is too old.  nbdkit >= %s is required.")
          nbdkit_min_version_string

let error_unless_nbdkit_plugin_exists plugin =
  if not (Nbdkit.probe_plugin plugin) then
    error (f_"nbdkit plugin %s is not installed") plugin

(* Check that nbdkit was compiled with SELinux support (for the
 * --selinux-label option).
 *)
let error_unless_nbdkit_compiled_with_selinux config =
  if have_selinux then (
    let selinux = try List.assoc "selinux" config with Not_found -> "no" in
    if selinux = "no" then
      error (f_"nbdkit was compiled without SELinux support.  You will have to recompile nbdkit with libselinux-devel installed, or else set SELinux to Permissive mode while doing the conversion.")
  )

let common_create ?bandwidth ?extra_debug ?extra_env plugin_name plugin_args =
  error_unless_nbdkit_working ();
  let config = Nbdkit.config () in
  error_unless_nbdkit_min_version config;
  error_unless_nbdkit_compiled_with_selinux config;

  (* Construct the nbdkit command. *)
  let cmd = Nbdkit.new_cmd in
  let cmd = Nbdkit.set_plugin cmd plugin_name in

  (* Environment.  We always add LANG=C. *)
  let cmd = Nbdkit.add_env cmd "LANG" "C" in
  let cmd =
    match extra_env with
    | None -> cmd
    | Some (name, value) -> Nbdkit.add_env cmd name value in

  (* Debug flags. *)
  let cmd =
    match extra_debug with
    | None -> cmd
    | Some (n, v) -> Nbdkit.add_debug_flag cmd n v in

  (* Other flags. *)
  let cmd = Nbdkit.set_verbose cmd (verbose ()) in
  let cmd = Nbdkit.set_readonly cmd true in (* important! readonly mode *)
  let cmd = Nbdkit.set_exportname cmd "/" in
  let cmd =
    if have_selinux then
      (* Label the socket so qemu can open it. *)
      Nbdkit.set_selinux_label cmd (Some "system_u:object_r:svirt_socket_t:s0")
    else cmd in

  (* Retry filter (if it exists) can be used to get around brief
   * interruptions in service.  It must be closest to the plugin.
   *)
  let cmd = Nbdkit.add_filter_if_available cmd "retry" in

  (* Adding the readahead filter is always a win for our access
   * patterns.  However if it doesn't exist don't worry.
   *)
  let cmd = Nbdkit.add_filter_if_available cmd "readahead" in

  (* Caching extents speeds up qemu-img, especially its consecutive
   * block_status requests with req_one=1.
   *)
  let cmd = Nbdkit.add_filter_if_available cmd "cacheextents" in

  (* Add the rate filter.  This must be furthest away so that
   * we don't end up rate-limiting internal nbdkit operations.
   *)
  let cmd, rate_args =
    if Nbdkit.probe_filter "rate" then (
      match bandwidth with
      | None -> cmd, []
      | Some bandwidth ->
         let cmd = Nbdkit.add_filter cmd "rate" in
         let args =
           match bandwidth with
           | StaticBandwidth rate ->
              [ "rate=", rate ]
           | DynamicBandwidth (None, filename) ->
              [ "rate-file=", filename ]
           | DynamicBandwidth (Some rate, filename) ->
              [ "rate=", rate; "rate-file=", filename ] in
         cmd, args
    )
    else cmd, [] in

  (* Adds the plugin and filter args. *)
  let cmd =
    List.fold_left (fun cmd (k, v) -> Nbdkit.add_arg cmd k v)
      cmd (plugin_args @ rate_args) in

  cmd

(* VDDK libraries are located under lib32/ or lib64/ relative to the
 * libdir.  Note this is unrelated to Linux multilib or multiarch.
 *)
let libNN = sprintf "lib%d" Sys.word_size

(* Create an nbdkit module specialized for reading from VDDK sources. *)
let create_vddk ?bandwidth ?config ?cookie ?libdir ~moref
                ?nfchostport ?password_file ?port
                ~server ?snapshot ~thumbprint ?transports ?user path =
  error_unless_nbdkit_plugin_exists "vddk";

  let version = Nbdkit.version (Nbdkit.config ()) in

  (* Compute the LD_LIBRARY_PATH that we may have to pass to nbdkit. *)
  let ld_library_path =
    (* LD_LIBRARY_PATH was only required by nbdkit < 1.17.10. *)
    if version >= (1, 17, 10) then None
    else Option.map (fun libdir -> libdir // libNN) libdir in
  let env =
    match ld_library_path with
    | None -> None
    | Some ld_library_path -> Some ("LD_LIBRARY_PATH", ld_library_path) in

  (* Check that the VDDK path looks reasonable. *)
  let error_unless_vddk_libdir () =
    (match libdir with
     | None -> ()
     | Some libdir ->
        if not (is_directory libdir) then
          error (f_"‘-io vddk-libdir=%s’ does not point to a directory.  See the virt-v2v-input-vmware(1) manual.") libdir
    );

    (match ld_library_path with
     | None -> ()
     | Some ld_library_path ->
        if not (is_directory ld_library_path) then
          error (f_"VDDK library path %s not found or not a directory.  See the virt-v2v-input-vmware(1) manual.") ld_library_path
    )
  in

  (* Check that the VDDK plugin is installed and working.  We also
   * check this later when calling common_create, but this version
   * has better troubleshooting output.
   *)
  let error_unless_nbdkit_vddk_working () =
    let env_as_string =
      match env with
      | None -> ""
      | Some (k, v) -> sprintf "%s=%s " k (quote v) in
    let cmd =
      sprintf "%snbdkit vddk --dump-plugin >/dev/null" env_as_string in
    if Sys.command cmd <> 0 then (
      (* See if we can diagnose why ... *)
      let cmd =
        sprintf "LANG=C %snbdkit vddk --dump-plugin 2>&1 |
                     grep -sq \"cannot open shared object file\""
                env_as_string in
      let needs_library = Sys.command cmd = 0 in
      if not needs_library then
        error (f_"nbdkit VDDK plugin is not installed or not working.  It is required if you want to use VDDK.

The VDDK plugin is not enabled by default when you compile nbdkit.  You have to read the instructions in the nbdkit sources under ‘plugins/vddk/README.VDDK’ to find out how to enable the VDDK plugin.

See also the virt-v2v-input-vmware(1) manual.")
      else
        error (f_"nbdkit VDDK plugin is not installed or not working.  It is required if you want to use VDDK.

It looks like you did not set the right path in the ‘-io vddk-libdir’ option, or your copy of the VDDK directory is incomplete.  There should be a library called ’<libdir>/%s/libvixDiskLib.so.?’.

See also the virt-v2v-input-vmware(1) manual.") libNN
    )
  in

  error_unless_vddk_libdir ();
  error_unless_nbdkit_vddk_working ();

  (* For VDDK we require some user.  If it's not supplied, assume root. *)
  let user = Option.default "root" user in

  let add_arg, get_args =
    let args = ref [] in
    let add_arg (k, v) = List.push_front (k, v) args in
    let get_args () = List.rev !args in
    add_arg, get_args in

  let password_param =
    match password_file with
    | None ->
       (* nbdkit asks for the password interactively *)
       "password", "-"
    | Some password_file ->
       (* nbdkit reads the password from the file *)
       "password", "+" ^ password_file in
  add_arg ("server", server);
  add_arg ("user", user);
  add_arg password_param;
  add_arg ("vm", sprintf "moref=%s" moref);
  add_arg ("file", path);

  (* The passthrough parameters. *)
  Option.may (fun s -> add_arg ("config", s)) config;
  Option.may (fun s -> add_arg ("cookie", s)) cookie;
  Option.may (fun s -> add_arg ("libdir", s)) libdir;
  Option.may (fun s -> add_arg ("nfchostport", s)) nfchostport;
  Option.may (fun s -> add_arg ("port", s)) port;
  Option.may (fun s -> add_arg ("snapshot", s)) snapshot;
  add_arg ("thumbprint", thumbprint);
  Option.may (fun s -> add_arg ("transports", s)) transports;

  (* If nbdkit >= 1.17.10 then we can suppress datapath messages. *)
  let debug_flag =
    if version >= (1, 17, 10) then Some ("vddk.datapath", "0") else None in

  common_create ?bandwidth ?extra_debug:debug_flag ?extra_env:env
    "vddk" (get_args ())

(* Create an nbdkit module specialized for reading from SSH sources. *)
let create_ssh ?bandwidth ~password ?port ~server ?user path =
  error_unless_nbdkit_plugin_exists "ssh";

  let add_arg, get_args =
    let args = ref [] in
    let add_arg (k, v) = List.push_front (k, v) args in
    let get_args () = List.rev !args in
    add_arg, get_args in

  add_arg ("host", server);
  Option.may (fun s -> add_arg ("port", s)) port;
  Option.may (fun s -> add_arg ("user", s)) user;
  (match password with
   | NoPassword -> ()
   | AskForPassword -> add_arg ("password", "-")
   | PasswordFile password_file -> add_arg ("password", "+" ^ password_file)
  );
  add_arg ("path", path);

  common_create ?bandwidth "ssh" (get_args ())

(* Create an nbdkit module specialized for reading from Curl sources. *)
let create_curl ?bandwidth ?cookie ~password ?(sslverify=true) ?user url =
  error_unless_nbdkit_plugin_exists "curl";

  let add_arg, get_args =
    let args = ref [] in
    let add_arg (k, v) = List.push_front (k, v) args in
    let get_args () = List.rev !args in
    add_arg, get_args in

  Option.may (fun s -> add_arg ("user", s)) user;
  (match password with
   | NoPassword -> ()
   | AskForPassword -> add_arg ("password", "-")
   | PasswordFile password_file -> add_arg ("password", "+" ^ password_file)
  );
  (* https://bugzilla.redhat.com/show_bug.cgi?id=1146007#c10 *)
  add_arg ("timeout", "2000");
  Option.may (fun s -> add_arg ("cookie", s)) cookie;
  if not sslverify then add_arg ("sslverify", "false");
  add_arg ("url", url);

  common_create ?bandwidth "curl" (get_args ())

let run cmd =
  let sock, _ = Nbdkit.run_unix cmd in

  if have_selinux then (
    (* Note that Unix domain sockets have both a file label and
     * a socket/process label.  Using --selinux-label only set
     * the socket label, but we must also set the file label.
     *)
    ignore (
        run_command ["chcon"; "system_u:object_r:svirt_image_t:s0"; sock]
      );
  );

  let qemu_uri = sprintf "nbd:unix:%s:exportname=/" sock in
  qemu_uri
