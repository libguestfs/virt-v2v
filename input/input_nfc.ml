(* helper-v2v-input
 * Copyright (C) 2009-2026 Red Hat Inc.
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

module NFC = struct
  let to_string options args =
    let xs = "-it nfc" :: args in
    let xs =
      match options.input_conn with
      | Some ic -> ("-ic " ^ ic) :: xs
      | None -> xs in
    let xs = "-i libvirt" :: xs in
    String.concat " " xs

  let query_input_options () =
    printf (f_"Input options (-io) which can be used with -it nfc
(all settings are optional):

  -io nfc-cookie=COOKIE        NFC cookie
  -io nfc-file=FILE            Override nbdkit-nfc-plugin file= parameter
  -io nfc-nfchostport=PORT     NFC nfchostport
  -io nfc-port=PORT            NFC port
  -io nfc-snapshot=SNAPSHOT-MOREF
                               NFC snapshot moref
  -io nfc-thumbprint=xx:xx:xx:...
                               NFC server thumbprint

Refer to nbdkit-nfc-plugin documentation for further information on
these settings.
")

  let rec setup dir options args =
    (* Check there are no input options we don't understand.
     * Also removes the "nfc-" prefix from the internal list.
     *)
    let nfc_option_keys =
      [ "cookie";
        "file";
        "nfchostport";
        "port";
        "snapshot";
        "thumbprint" ] in

    let io_options =
      List.map (
        fun (key, value) ->
          let error_invalid_key () =
            error (f_"-it nfc: ‘-io %s’ is not a valid input option") key
          in
          if not (String.starts_with "nfc-" key) then error_invalid_key ();
          let key = String.sub key 4 (String.length key-4) in
          if not (List.mem key nfc_option_keys) then error_invalid_key ();
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
      error (f_"in-place mode does not work with NFC (-it nfc) source");

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

    (* Find the <vmware:moref> element from the XML.  This was added
     * in libvirt >= 3.7 and is required.
     *)
    let moref =
      let doc = Xml.parse_memory xml in
      let xpathctx = Xml.xpath_new_context doc in
      Xml.xpath_register_ns xpathctx
        "vmware" "http://libvirt.org/schemas/domain/vmware/1.0";
      let xpath_string = xpath_string xpathctx in
      match xpath_string "/domain/vmware:moref" with
      | Some moref -> moref
      | None ->
         error (f_"<vmware:moref> was not found in the output of \
                   ‘virsh dumpxml \"%s\"’.  The most likely reason is that \
                   libvirt is too old, try upgrading \
                   libvirt to ≥ 3.7.") guest in

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

    (* For NFC we require some user.  If it's not supplied, assume root. *)
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
         printf (f_"%s: enter password for ‘%s’: ") "nfc" user;
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

    let cookie =
      try Some (List.assoc "cookie" io_options) with Not_found -> None in
    let nfchostport =
      try Some (List.assoc "nfchostport" io_options) with Not_found -> None in
    let port =
      try Some (List.assoc "port" io_options) with Not_found -> None in
    let snapshot =
      try Some (List.assoc "snapshot" io_options) with Not_found -> None in
    let thumbprint =
      try Some (List.assoc "thumbprint" io_options) with Not_found -> None in

    (* If -io nfc-file was given, there must be exactly one per guest
     * disk.  Get the list of file overrides.
     *)
    let file_overrides =
      if List.mem_assoc "file" io_options then (
        let fos =
          List.filter_map (function ("file",b) -> Some (Some b) | _ -> None)
            io_options in
        if List.length fos <> nr_disks then
          error (f_"‘-io nfc-file=’ must be used exactly %d times") nr_disks;
        fos
      )
      else (
        (* List of no overrides. *)
        List.make nr_disks None
      ) in

    (* Check we have nbdkit and the nfc plugin and the cow filter. *)
    if not (Nbdkit.is_installed ()) then
      error (f_"nbdkit is not installed or not working");
    if not (Nbdkit.probe_plugin "nfc") then
      error (f_"nbdkit-nfc-plugin is not installed");
    if not (Nbdkit.probe_filter "cow") then
      error (f_"nbdkit-cow-filter is not installed or not working");

    (* Check that nbdkit-nfc-plugin is installed and working.  We also
     * check this later when calling common_create, but this version
     * has better troubleshooting output.
     *)
    let error_unless_nbdkit_nfc_working () =
      let cmd = "nbdkit nfc --dump-plugin >/dev/null" in
      if Sys.command cmd <> 0 then (
          error (f_"nbdkit nfc plugin is not installed or not working.  It is required if you want to use ‘-it nfc’.

See also the virt-v2v-input-vmware(1) manual.")
      )
    in

    error_unless_nbdkit_nfc_working ();

    (* Helper to create an nbdkit command object. *)
    let create_nbdkit_nfc ?name () =
      let cmd = Nbdkit.create ?name "nfc" in

      Nbdkit.add_arg cmd "server" server;
      (* I'm pretty sure this was the default with the old transport mode.
       * Maybe it should be settable via -io?
       *)
      Nbdkit.add_arg cmd "insecure" "true";

      Nbdkit.add_arg cmd "vm" (sprintf "moref=%s" moref);

      (* NFC requires user and password parameters. *)
      Nbdkit.add_arg cmd "user" user;
      Nbdkit.add_arg cmd "password" ("+" ^ password_file);

      (* The passthrough parameters. *)
      let passthru cmd name v = Option.iter (Nbdkit.add_arg cmd name) v in
      passthru cmd "cookie" cookie;
      passthru cmd "nfchostport" nfchostport;
      passthru cmd "port" port;
      passthru cmd "snapshot" snapshot;
      passthru cmd "thumbprint" thumbprint;

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

(* XXX Check if this is necessary with the NFC plugin

      if Nbdkit.probe_filter "multi-conn" then (
        Nbdkit.add_filter cmd "multi-conn";
        Nbdkit.add_arg cmd "multi-conn-mode" "disable";
      );
*)

      (* If the filter supports it, enable cow-block-size (added in
       * nbdkit 1.27.6).  This helps to reduce fragmentated small
       * extent and read requests.
       *)
      if Nbdkit.probe_filter_parameter "cow" "cow-block-size" then
        Nbdkit.add_arg cmd "cow-block-size" "4096";

      (* Add the cow-on-read flag if supported. *)
      if Nbdkit.probe_filter_parameter "cow" "cow-on-read=.*/PATH" then
        Nbdkit.add_arg cmd "cow-on-read" (dir // "convert");

      cmd
    in

    (* Collect all the filename(s) we will be accessing, one
     * for each input disk.  This takes into account the
     * '-io nfc-file' override.
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
              * directly in this form to NFC.
              *)
             file

          | { d_type = LocalFile _ }, Some file_override ->
             (* If -io nfc-file, override it here. *)
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

          let nbdkit = create_nbdkit_nfc ~name:sockname () in
          Nbdkit.add_arg nbdkit "file" file;
          let _, pid = Nbdkit.run_unix socket nbdkit in
          On_exit.kill pid;

          NBD_URI.Unix (socket, None)
      ) files in

    source, uris
end
