(* helper-v2v-input
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
open Xpath_helpers

open Types
open Utils

open Parse_libvirt_xml
open Input

module PyNFC = struct
  let to_string options args =
    let xs = "-it pynfc" :: args in
    let xs =
      match options.input_conn with
      | Some ic -> ("-ic " ^ ic) :: xs
      | None -> xs in
    let xs = "-i libvirt" :: xs in
    String.concat " " xs

  let query_input_options () =
    printf (f_"Input options (-io) which can be used with -it pynfc
(all settings are optional):

  -io pynfc-file=FILE          Override nbdkit-pynfc-plugin file= parameter
  -io pynfc-port=PORT          API port (default: 443)

Refer to nbdkit-pynfc-plugin(1) for further information on these settings.
")

  let rec setup dir options args =
    (* Check there are no input options we don't understand.
     * Also removes the "pynfc-" prefix from the internal list.
     *)
    let pynfc_option_keys =
      [ "file";
        "port" ] in

    let io_options =
      List.map (
        fun (key, value) ->
          let error_invalid_key () =
            error (f_"-it pynfc: ‘-io %s’ is not a valid input option") key
          in
          if not (String.starts_with "pynfc-" key) then error_invalid_key ();
          let key = String.sub key 6 (String.length key-6) in
          if not (List.mem key pynfc_option_keys) then error_invalid_key ();
          (key, value)
      ) options.input_options in

    (* Get the guest name. *)
    let guest =
      match args with
      | [arg] -> arg
      | _ ->
         error (f_"-i libvirt: expecting a libvirt guest name \
                   on the command line") in

    (* -ic must be set and it must contain a server.  This is
     * enforced by virt-v2v.
     *)
    let input_conn =
      match options.input_conn with
      | Some ic -> ic
      | None ->
         error (f_"-i libvirt: expecting -ic parameter \
                   for vcenter connection") in

    if not options.read_only then
      error (f_"in-place mode does not work with pynfc source");

    let uri =
      try Xml.parse_uri input_conn
      with Invalid_argument msg ->
        error (f_"could not parse '-ic %s'.  Original error message was: %s")
          input_conn msg in

    (* Connect to the hypervisor. *)
    let conn =
      let auth = Libvirt_utils.auth_for_password_file
                   ?password_file:options.input_password () in
      Libvirt.Connect.connect_auth ~name:input_conn auth in

    (* Parse the libvirt XML. *)
    let source, disks, xml = parse_libvirt_domain conn guest in
    let nr_disks = List.length disks in

    (* It probably never happens that the server name can be missing
     * from the libvirt URI, but we need a server name to pass to
     * nbdkit, so ...
     *)
    let server =
      match uri.Xml.uri_server with
      | Some server -> server
      | None ->
         error (f_"‘-ic %s’ URL does not contain a host name field")
           input_conn in

    (* We require some user.  If it's not supplied, assume root. *)
    let user = uri.Xml.uri_user |> Option.value ~default:"root" in

    (* If asking for a password, do it now. *)
    let password_file =
      match options.input_password with
      | None ->
         (* Because we will start nbdkit in the background and then wait
          * for 30 seconds for it to start up, we cannot use the
          * password=- feature of nbdkit to read the password
          * interactively (since in the words of the movie the user has
          * only "30 seconds to comply").  So in the
          * AskForPassword case we read the password here.
          *)
         printf (f_"%s: enter password for ‘%s’: ") "pynfc" user;
         let open Unix in
         let orig = tcgetattr stdin in
         let tios = { orig with c_echo = false } in
         tcsetattr stdin TCSAFLUSH tios; (* Disable echo. *)
         let password = read_line () in
         tcsetattr stdin TCSAFLUSH orig; (* Restore echo. *)
         printf "\n%!";
         let password_file = Filename.temp_file "v2vnbdkit" ".txt" in
         with_open_out password_file (fun chan -> output_string chan password);
         On_exit.unlink password_file;
         password_file

      | Some password_file ->
         password_file in

    let port =
      try Some (List.assoc "port" io_options) with Not_found -> None in

    (* If -io pynfc-file was given, there must be exactly one per guest
     * disk.  Get the list of file overrides.
     *)
    let file_overrides =
      if List.mem_assoc "file" io_options then (
        let fos =
          List.filter_map (function ("file",b) -> Some (Some b) | _ -> None)
            io_options in
        if List.length fos <> nr_disks then
          error (f_"‘-io pynfc-file=’ must be used exactly %d times") nr_disks;
        fos
      )
      else (
        (* List of no overrides. *)
        List.make nr_disks None
      ) in

    (* Check we have nbdkit and the pynfc plugin and the cow filter. *)
    if not (Nbdkit.is_installed ()) then
      error (f_"nbdkit is not installed or not working");
    if not (Nbdkit.version () >= (1, 45, 11)) then
      error (f_"nbdkit must be >= 1.45.11 for input from pynfc");
    if not (Nbdkit.probe_plugin "pynfc") then
      error (f_"nbdkit-pynfc-plugin is not installed");
    if not (Nbdkit.probe_filter "cow") then
      error (f_"nbdkit-cow-filter is not installed or not working");

    (* Check that the pynfc plugin is installed and working.  We also
     * check this later when calling common_create, but this version
     * has better troubleshooting output.
     *)
    let error_unless_nbdkit_pynfc_working () =
      let cmd = "nbdkit pynfc --dump-plugin >/dev/null" in
      if Sys.command cmd <> 0 then (
        error (f_"nbdkit pynfc plugin is not installed or not working.  It is required if you want to use ‘-it pynfc’.

See the virt-v2v-input-vmware(1) manual.")
      )
    in

    error_unless_nbdkit_pynfc_working ();

    (* Helper to create an nbdkit command object. *)
    let create_nbdkit ?name () =
      let plugin = "pynfc" in
      let cmd = Nbdkit.create ?name plugin in

      Nbdkit.add_arg cmd "server" server;
      Nbdkit.add_arg cmd "verify" "false";
      Nbdkit.add_arg cmd "vm" guest; (* XXX read from domain XML? *)

      (* VMware requires user and password parameters. *)
      Nbdkit.add_arg cmd "user" user;
      Nbdkit.add_arg cmd "password" ("+" ^ password_file);

      (* The passthrough parameters. *)
      let passthru cmd name v = Option.iter (Nbdkit.add_arg cmd name) v in
      passthru cmd "port" port;

      (* Retry filter (if it exists) can be used to get around brief
       * interruptions in service.  It must be closest to the plugin.
       *)
      Nbdkit.add_filter_if_available cmd "retry";

      (* Add the count filter if available, to report bytes read.
       * Since it writes a debug message, only do this if verbose.
       * This should be close to the plugin so we're reporting what
       * is read over the wire.
       *)
      if verbose () then Nbdkit.add_filter_if_available cmd "count";

(*
      (* Split very large requests to avoid out of memory errors on the
       * server.  Since we're using this filter, also add minblock=512
       * although it will make no difference.
       *)
      if Nbdkit.probe_filter "blocksize" then (
        Nbdkit.add_filter cmd "blocksize";
        Nbdkit.add_arg cmd "minblock" "512";
        Nbdkit.add_arg cmd "maxdata" "2M"
      );
*)

      (* IMPORTANT! Add the COW filter.  It must be furthest away
       * except for the multi-conn and rate filters.
       *)
      Nbdkit.add_filter cmd "cow";

      (* Force no-multi-conn, even though the cow filter enables it. *)
      if Nbdkit.probe_filter "multi-conn" then (
        Nbdkit.add_filter cmd "multi-conn";
        Nbdkit.add_arg cmd "multi-conn-mode" "disable";
      );

      (* If the filter supports it, enable cow-block-size (added in
       * nbdkit 1.27.6).  This helps to reduce fragmentated small
       * extent and read requests.
       *)
      if Nbdkit.probe_filter_parameter "cow" "cow-block-size" then
        Nbdkit.add_arg cmd "cow-block-size" "4096";

      (* Add the cow-on-read flag if supported. *)
      if Nbdkit.probe_filter_parameter "cow" "cow-on-read=.*/PATH" then
        Nbdkit.add_arg cmd "cow-on-read" (dir // "convert");

      (* Add the rate filter.  This must be furthest away so that
       * we don't end up rate-limiting internal nbdkit operations.
       *)
      if Nbdkit.probe_filter "rate" then (
        match options.bandwidth with
        | None -> ()
        | Some bandwidth ->
           Nbdkit.add_filter cmd "rate";
           match bandwidth with
           | StaticBandwidth rate ->
              Nbdkit.add_arg cmd "rate" rate
           | DynamicBandwidth (None, filename) ->
              Nbdkit.add_arg cmd "rate-file" filename
           | DynamicBandwidth (Some rate, filename) ->
              Nbdkit.add_args cmd ["rate", rate; "rate-file", filename]
      );

      cmd
    in

    (* Collect all the filename(s) we will be accessing, one
     * for each input disk.  This takes into account the
     * '-io pynfc-file' override.
     *)
    let files =
      List.combine disks file_overrides |>
      List.map (
        function
        (* These should never happen? *)
          | { d_type = BlockDev _ | NBD _ | HTTP _ }, _ ->
             assert false

          | { d_type = LocalFile file }, None ->
             (* The <source file=...> attribute returned by the libvirt
              * VMX driver looks like "[datastore] path".  We can use it
              * directly as the nbdkit file= parameter, and it is passed
              * directly in this form to pynfc.
              *)
             file

          | { d_type = LocalFile _ }, Some file_override ->
             (* If -io pynfc-file, override it here. *)
             file_override
      ) in
    assert (files <> []);

    (* Create an nbdkit instance for each disk. *)
    let uris =
      List.mapi (
        fun i file ->
          let sockname = sprintf "in%d" i in
          let socket = sprintf "%s/%s" dir sockname in
          On_exit.unlink socket;

          let nbdkit = create_nbdkit ~name:sockname () in
          Nbdkit.add_arg nbdkit "file" file;
          let _, pid = Nbdkit.run_unix socket nbdkit in
          On_exit.kill pid;

          NBD_URI.Unix (socket, None)
      ) files in

    source, uris
end
