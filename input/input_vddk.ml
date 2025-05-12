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

(* VDDK libraries are located under lib32/ or lib64/ relative to the
 * libdir.  Note this is unrelated to Linux multilib or multiarch.
 *)
let libNN = sprintf "lib%d" Sys.word_size

module VDDK = struct
  let to_string options args =
    let xs = "-it vddk" :: args in
    let xs =
      match options.input_conn with
      | Some ic -> ("-ic " ^ ic) :: xs
      | None -> xs in
    let xs = "-i libvirt" :: xs in
    String.concat " " xs

  let query_input_options () =
    printf (f_"Input options (-io) which can be used with -it vddk:

  -io vddk-thumbprint=xx:xx:xx:...
                               VDDK server thumbprint (required)

All other settings are optional:

  -io vddk-config=FILE         VDDK configuration file
  -io vddk-cookie=COOKIE       VDDK cookie
  -io vddk-file=FILE           Override nbdkit-vddk-plugin file= parameter
  -io vddk-libdir=LIBDIR       VDDK library parent directory
  -io vddk-nfchostport=PORT    VDDK nfchostport
  -io vddk-noextents=true      Avoid slow VDDK QueryAllocatedBlocks API
  -io vddk-port=PORT           VDDK port
  -io vddk-snapshot=SNAPSHOT-MOREF
                               VDDK snapshot moref
  -io vddk-transports=MODE:MODE:..
                               VDDK transports

Refer to nbdkit-vddk-plugin(1) and the VDDK documentation for further
information on these settings.
")

  let rec setup dir options args =
    (* Check there are no input options we don't understand.
     * Also removes the "vddk-" prefix from the internal list.
     *)
    let vddk_option_keys =
      [ "config";
        "cookie";
        "file";
        "libdir";
        "nfchostport";
        "noextents";
        "port";
        "snapshot";
        "thumbprint";
        "transports" ] in

    let io_options =
      List.map (
        fun (key, value) ->
          let error_invalid_key () =
            error (f_"-it vddk: ‘-io %s’ is not a valid input option") key
          in
          if not (String.starts_with "vddk-" key) then error_invalid_key ();
          let key = String.sub key 5 (String.length key-5) in
          if not (List.mem key vddk_option_keys) then error_invalid_key ();
          (key, value)
      ) options.input_options in

    (* thumbprint is mandatory. *)
    if not (List.mem_assoc "thumbprint" io_options) then
      error (f_"You must pass the ‘-io vddk-thumbprint’ option with the \
                SSL thumbprint of the VMware server.  To find the thumbprint, \
                see the nbdkit-vddk-plugin(1) manual.  See also the \
                virt-v2v-input-vmware(1) manual.");

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
      error (f_"in-place mode does not work with VDDK source");

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
         error (f_"‘-ic %s’ URL does not contain a host name field") input_conn in

    (* For VDDK we require some user.  If it's not supplied, assume root. *)
    let user = uri.Xml.uri_user |> Option.value ~default:"root" in

    (* If asking for a password, do it now. *)
    let password_file =
      match options.input_password with
      | None ->
         (* Because we will start nbdkit in the background and then wait
          * for 30 seconds for it to start up, we cannot use the
          * password=- feature of nbdkit to read the password
          * interactively (since in the words of the movie the user has
          * only "30 seconds to comply").  In any case this feature broke
          * in the VDDK plugin in nbdkit 1.18 and 1.20.  So in the
          * AskForPassword case we read the password here.
          *)
         printf (f_"%s: enter password for ‘%s’: ") "vddk" user;
         let open Unix in
         let orig = tcgetattr stdin in
         let tios = { orig with c_echo = false } in
         tcsetattr stdin TCSAFLUSH tios; (* Disable echo. *)
         let password = read_line () in
         tcsetattr stdin TCSAFLUSH orig; (* Restore echo. *)
         printf "\n";
         let password_file = Filename.temp_file "v2vnbdkit" ".txt" in
         with_open_out password_file (fun chan -> output_string chan password);
         On_exit.unlink password_file;
         password_file

      | Some password_file ->
         password_file in

    let config =
      try Some (List.assoc "config" io_options) with Not_found -> None in
    let cookie =
      try Some (List.assoc "cookie" io_options) with Not_found -> None in
    let libdir =
      try Some (List.assoc "libdir" io_options) with Not_found -> None in
    let nfchostport =
      try Some (List.assoc "nfchostport" io_options) with Not_found -> None in
    let noextents =
      try bool_of_string (List.assoc "noextents" io_options)
      with Not_found -> false in
    let port =
      try Some (List.assoc "port" io_options) with Not_found -> None in
    let snapshot =
      try Some (List.assoc "snapshot" io_options) with Not_found -> None in
    let thumbprint =
      try List.assoc "thumbprint" io_options
      with Not_found -> assert false (* checked above *) in
    let transports =
      try Some (List.assoc "transports" io_options) with Not_found -> None in

    (* If -io vddk-file was given, there must be exactly one per guest
     * disk.  Get the list of file overrides.
     *)
    let file_overrides =
      if List.mem_assoc "file" io_options then (
        let fos =
          List.filter_map (function ("file",b) -> Some (Some b) | _ -> None)
            io_options in
        if List.length fos <> nr_disks then
          error (f_"‘-io vddk-file=’ must be used exactly %d times") nr_disks;
        fos
      )
      else (
        (* List of no overrides. *)
        List.make nr_disks None
      ) in

    (* Check we have nbdkit and the vddk plugin and the cow filter. *)
    if not (Nbdkit.is_installed ()) then
      error (f_"nbdkit is not installed or not working");
    if not (Nbdkit.probe_plugin "vddk") then
      error (f_"nbdkit-vddk-plugin is not installed");
    if not (Nbdkit.probe_filter "cow") then
      error (f_"nbdkit-cow-filter is not installed or not working");

    (* Check that the VDDK libdir looks reasonable. *)
    let error_unless_vddk_libdir () =
      match libdir with
      | None -> ()
      | Some libdir ->
         if not (is_directory libdir) then
           error (f_"‘-io vddk-libdir=%s’ does not point to a directory.  \
                     See the virt-v2v-input-vmware(1) manual.") libdir
    in

    (* Check that the VDDK plugin is installed and working.  We also
     * check this later when calling common_create, but this version
     * has better troubleshooting output.
     *)
    let error_unless_nbdkit_vddk_working () =
      let cmd = "nbdkit vddk --dump-plugin >/dev/null" in
      if Sys.command cmd <> 0 then (
        (* See if we can diagnose why ... *)
        let cmd = "LANG=C nbdkit vddk --dump-plugin 2>&1 |
                   grep -sq \"cannot open shared object file\"" in
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

    (* Helper to create an nbdkit command object. *)
    let create_nbdkit_vddk () =
      let cmd = Nbdkit.create "vddk" in

      (* Suppress datapath messages. *)
      Nbdkit.add_debug_flag cmd "vddk.datapath" "0";

      (* Enable VDDK stats. *)
      Nbdkit.add_debug_flag cmd "vddk.stats" "1";

      Nbdkit.add_arg cmd "server" server;
      Nbdkit.add_arg cmd "vm" (sprintf "moref=%s" moref);

      (* VDDK requires user and password parameters. *)
      Nbdkit.add_arg cmd "user" user;
      Nbdkit.add_arg cmd "password" ("+" ^ password_file);

      (* The passthrough parameters. *)
      let passthru cmd name v = Option.iter (Nbdkit.add_arg cmd name) v in
      passthru cmd "config" config;
      passthru cmd "cookie" cookie;
      passthru cmd "libdir" libdir;
      passthru cmd "nfchostport" nfchostport;
      passthru cmd "port" port;
      passthru cmd "snapshot" snapshot;
      Nbdkit.add_arg cmd "thumbprint" thumbprint; (* required *)
      passthru cmd "transports" transports;

      (* Retry filter (if it exists) can be used to get around brief
       * interruptions in service.  It must be closest to the plugin.
       *)
      Nbdkit.add_filter_if_available cmd "retry";

      (* VDDK's QueryAllocatedBlocks API is infamously slow.  It appears
       * to block all other requests while it is running.  This API is
       * also only called during the copy phase, not during conversion
       * (or if it is, extremely rarely).
       *
       * If fstrim was successful, then trimmed blocks are stored in
       * the COW filter (see below), and so requests for extents stop
       * at that layer.  However for areas of the disk that fstrim
       * thinks contain data, we still have to go through to VDDK to
       * fetch extents.
       *
       * We could therefore add nbdkit-noextents-filter here (below COW,
       * above VDDK plugin) which stops extents requests from going
       * to VDDK, which would stop QueryAllocatedBlocks ever being
       * called.  In my testing this is a moderate performance win.
       *
       * However ... in the case where fstrim failed, or for filesystems
       * or partitions on the disk that we don't understand, doing this
       * would mean that those are copied completely, as there would be
       * no extent data (nbdcopy will still sparsify them on the target,
       * but we'd have to copy all the bits from VMware).  Because
       * here we don't know if this is the case, be conservative and
       * actually don't use this filter.
       *
       * If used, this filter should be close to the plugin and MUST
       * be below the COW filter.
       *)
      if noextents then
        Nbdkit.add_filter_if_available cmd "noextents";

      (* Split very large requests to avoid out of memory errors on the
       * server.  Since we're using this filter, also add minblock=512
       * although it will make no difference.
       *)
      if Nbdkit.probe_filter "blocksize" then (
        Nbdkit.add_filter cmd "blocksize";
        Nbdkit.add_arg cmd "minblock" "512";
        Nbdkit.add_arg cmd "maxdata" "2M"
      );

      (* IMPORTANT! Add the COW filter.  It must be furthest away
       * except for the multi-conn and rate filters.
       *)
      Nbdkit.add_filter cmd "cow";

      (* The cow filter unconditionally enables multi-conn (because it is
       * safe).  However this causes an unintended consequence with the VDDK
       * plugin.  Multiple VDDK handles are opened (one per multi-conn
       * connection), and for some reason, possibly internal locking, they
       * conflict with each other.  This manifests itself as API calls taking
       * between 2 and 7 times longer to serve (especially QueryAllocatedBlocks
       * which seems to slow down most).
       *
       * Avoid this by adding nbdkit-multi-conn-filter with
       * multi-conn-mode=disable on top which disables multi-conn
       * advertisement.
       *)
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

    (* Create an nbdkit instance for each disk. *)
    let uris =
      List.combine disks file_overrides |>
      List.mapi (
        fun i ({ d_format = format; d_type }, file_override) ->
          let socket = sprintf "%s/in%d" dir i in
          On_exit.unlink socket;

          (match d_type with
           | BlockDev _ | NBD _ | HTTP _ -> (* These should never happen? *)
              assert false

           | LocalFile orig_file ->
              (* If -io vddk-file, override it here. *)
              let file = Option.value file_override ~default:orig_file in

              (* The <source file=...> attribute returned by the libvirt
               * VMX driver looks like "[datastore] path".  We can use it
               * directly as the nbdkit file= parameter, and it is passed
               * directly in this form to VDDK.
               *)
              let nbdkit = create_nbdkit_vddk () in
              Nbdkit.add_arg nbdkit "file" file;
              let _, pid = Nbdkit.run_unix socket nbdkit in
              On_exit.kill pid
          );

          NBD_URI.Unix (socket, None)
      ) in

    source, uris
end
