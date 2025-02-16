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
open Unix_utils.Env
open Common_gettext.Gettext
open Xpath_helpers

open Types
open Utils

open Parse_libvirt_xml
open Input

module VCenterHTTPS = struct
  let to_string options args =
    let xs = args in
    let xs =
      match options.input_conn with
      | Some ic -> ("-ic " ^ ic) :: xs
      | None -> xs in
    let xs = "-i libvirt" :: xs in
    String.concat " " xs

  let query_input_options () =
    printf (f_"No input options can be used in this mode.\n")

  let rec setup dir options args =
    if options.input_options <> [] then
      error (f_"no -io (input options) are allowed here");

    if not options.read_only then
      error (f_"in-place mode does not work with HTTP source");

    (* Remove proxy environment variables so curl doesn't try to use
     * them.  Using a proxy is generally a bad idea because vCenter
     * is slow enough as it is without putting another device in
     * the way (RHBZ#1354507).
     *)
    unsetenv "https_proxy";
    unsetenv "all_proxy";
    unsetenv "no_proxy";
    unsetenv "HTTPS_PROXY";
    unsetenv "ALL_PROXY";
    unsetenv "NO_PROXY";

    let guest =
      match args with
      | [arg] -> arg
      | _ ->
         error (f_"-i libvirt: expecting a libvirt guest name \
                   on the command line") in

    (* -ip is required in this mode, see RHBZ#1960087 *)
    let password_file =
      match options.input_password with
      | Some file -> file
      | None ->
         error (f_"-i libvirt: expecting -ip passwordfile parameter for \
                   vCenter connection") in

    (* -ic must be set and it must contain a server.  This is
     * enforced by virt-v2v.
     *)
    let input_conn =
      match options.input_conn with
      | Some ic -> ic
      | None ->
         error (f_"-i libvirt: expecting -ic parameter for \
                   vcenter connection") in

    let uri =
      try Xml.parse_uri input_conn
      with Invalid_argument msg ->
        error (f_"could not parse '-ic %s'.  Original error message was: %s")
          input_conn msg in

    let server =
      match uri with
      | { Xml.uri_server = Some server } -> server
      | { Xml.uri_server = None } ->
         error (f_"-i libvirt: expecting -ic parameter to contain \
                   vcenter server name") in

    (* Connect to the hypervisor. *)
    let conn =
      let auth = Libvirt_utils.auth_for_password_file ~password_file () in
      Libvirt.Connect.connect_auth ~name:input_conn auth in

    (* Parse the libvirt XML. *)
    let source, disks, xml = parse_libvirt_domain conn guest in

    (* Find the <vmware:datacenterpath> element from the XML.  This
     * was added in libvirt >= 1.2.20.
     *)
    let dcPath =
      let doc = Xml.parse_memory xml in
      let xpathctx = Xml.xpath_new_context doc in
      Xml.xpath_register_ns xpathctx
        "vmware" "http://libvirt.org/schemas/domain/vmware/1.0";
      match xpath_string xpathctx "/domain/vmware:datacenterpath" with
      | Some dcPath -> dcPath
      | None ->
         error (f_"vcenter: <vmware:datacenterpath> was not found in the XML.  \
                   You need to upgrade to libvirt ≥ 1.2.20.") in

    List.iteri (
      fun i { d_format = format; d_type } ->
        let socket = sprintf "%s/in%d" dir i in
        On_exit.unlink socket;

        match d_type with
        | BlockDev _ | NBD _ | HTTP _ -> (* These should never happen? *)
           assert false

        | LocalFile path ->
           let cor = dir // "convert" in
           let pid = VCenter.start_nbdkit_for_path
                       ?bandwidth:options.bandwidth
                       ~cor ~password_file
                       dcPath uri server path socket in
           On_exit.kill pid
    ) disks;

    source
end
