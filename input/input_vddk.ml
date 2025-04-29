(* helper-v2v-input
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
open Xpath_helpers

open Types
open Utils

open Parse_libvirt_xml
open Input

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

  let setup dir options args =
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
          if not (String.is_prefix key "vddk-") then error_invalid_key ();
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

    let user = uri.Xml.uri_user in

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

    (* Create an nbdkit instance for each disk. *)
    List.combine disks file_overrides |>
    List.iteri (
      fun i ({ d_format = format; d_type }, file_override) ->
        let socket = sprintf "%s/in%d" dir i in
        On_exit.unlink socket;

        match d_type with
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
           let nbdkit =
             let cor = dir // "convert" in
             Nbdkit_vddk.create_vddk ?bandwidth:options.bandwidth
               ?config ?cookie ~cor
               ?libdir ~moref
               ?nfchostport ~noextents
               ?password_file:options.input_password ?port
               ~server ?snapshot ~thumbprint ?transports ?user
               file in
           let _, pid = Nbdkit.run_unix socket nbdkit in
           On_exit.kill pid
    );

    source
end
