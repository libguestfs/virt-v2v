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

open Types
open Utils

open Parse_libvirt_xml
open Input

module XenSSH = struct
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

  let setup dir options args =
    if options.input_options <> [] then
      error (f_"no -io (input options) are allowed here");

    if not options.read_only then
      error (f_"in-place mode does not work with Xen over SSH source");

    (* Get the guest name. *)
    let guest =
      match args with
      | [arg] -> arg
      | _ ->
         error (f_"-i libvirt: expecting a libvirt guest name \
                   on the command line") in

    (* -ic must be set. *)
    let input_conn =
      match options.input_conn with
      | Some ic -> ic
      | None ->
         error (f_"-i libvirt: expecting -ic parameter for \
                   Xen over SSH connection") in

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
    let source, disks, _ = parse_libvirt_domain conn guest in

    let server =
      match uri.Xml.uri_server with
      | Some server -> server
      | None ->
         error (f_"‘-ic %s’ URL does not contain a host name field")
           input_conn in

    let port =
      match uri.uri_port with
      | 0 | 22 -> None
      | i -> Some (string_of_int i) in

    let user = uri.uri_user in

    let password =
      match options.input_password with
      | None -> None
      | Some ip -> Some (Nbdkit_ssh.PasswordFile ip) in

    (* Create an nbdkit instance for each disk. *)
    List.iteri (
      fun i { d_format = format; d_type } ->
        let socket = sprintf "%s/in%d" dir i in
        On_exit.unlink socket;

        match d_type with
        | NBD _ | HTTP _ -> (* These should never happen? *)
           assert false

        | BlockDev _ ->
           (* Conversion from a remote block device over SSH isn't
            * supported because OpenSSH sftp server doesn't know how
            * to get the size of a block device.  Therefore we disallow
            * this and refer users to the manual.
            *)
           error (f_"input from xen over ssh does not support disks stored on \
                     remote block devices.  See virt-v2v-input-xen(1) \
                     section \"Xen or ssh conversions from block devices\".")

        | LocalFile path ->
           let cor = dir // "convert" in
           let bandwidth = options.bandwidth in
           let nbdkit = Nbdkit_ssh.create_ssh ?bandwidth ~cor ?password
                          ?port ~server ?user path in
           let _, pid = Nbdkit.run_unix socket nbdkit in
           On_exit.kill pid
    ) disks;

    source
end
