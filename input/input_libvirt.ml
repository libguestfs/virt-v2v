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

open Parse_libvirt_xml

open Types
open Utils

open Input

let rec get_source_from_libvirt options args =
  if options.input_options <> [] then
    error (f_"no -io (input options) are allowed here");

  let guest =
    match args with
    | [arg] -> arg
    | _ ->
       error (f_"-i libvirt: expecting a libvirt guest name \
                 on the command line") in

  (* Connect to the hypervisor. *)
  let conn =
    let auth = Libvirt_utils.auth_for_password_file
                 ?password_file:options.input_password () in
    Libvirt.Connect.connect_auth ?name:options.input_conn auth in

  (* Parse the libvirt XML. *)
  let source, disks, _ = parse_libvirt_domain conn guest in
  source, disks

and get_source_from_libvirt_xml _ args =
  let xmlfile =
    match args with
    | [arg] -> arg
    | _ ->
       error (f_"-i libvirtxml: expecting a libvirt XML filename \
                 on the command line") in
  let xml = read_whole_file xmlfile in
  let source, disks = parse_libvirt_xml xml in
  source, disks

and setup_servers options dir disks =
  (* Check nbdkit is installed. *)
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working.  It is required to \
              use ‘-i libvirt|libvirtxml’.");

  if not (Nbdkit.probe_plugin "file") then
    error (f_"nbdkit-file-plugin is not installed or not working");
  if not (Nbdkit.probe_plugin "nbd") then
    error (f_"nbdkit-nbd-plugin is not installed or not working");
  if options.read_only && not (Nbdkit.probe_filter "cow") then
    error (f_"nbdkit-cow-filter is not installed or not working");

  List.iteri (
    fun i { d_format = format; d_type } ->
      let socket = sprintf "%s/in%d" dir i in
      On_exit.unlink socket;

      match d_type with
      (* Forward to another NBD server using nbdkit-nbd-plugin. *)
      | NBD (hostname, port) ->
         let cmd = Nbdkit.create "nbd" in
         if options.read_only then
           Nbdkit.add_filter cmd "cow";
         Nbdkit.add_arg cmd "hostname" hostname;
         Nbdkit.add_arg cmd "port" (string_of_int port);
         Nbdkit.add_arg cmd "shared" "true";
         let _, pid = Nbdkit.run_unix socket cmd in

         (* --exit-with-parent should ensure nbdkit is cleaned
          * up when we exit, but it's not supported everywhere.
          *)
         On_exit.kill pid

      (* Forward to an HTTP/HTTPS server using nbdkit-curl-plugin. *)
      | HTTP url ->
         if not options.read_only then
           error (f_"in-place mode does not work with HTTP source");

         let cor = dir // "convert" in
         let cmd = Nbdkit_curl.create_curl ~cor url in
         let _, pid = Nbdkit.run_unix socket cmd in

         (* --exit-with-parent should ensure nbdkit is cleaned
          * up when we exit, but it's not supported everywhere.
          *)
         On_exit.kill pid

      | BlockDev filename | LocalFile filename ->
         match format with
         | Some "raw" ->
            let cmd = Nbdkit.create "file" in
            if options.read_only then
              Nbdkit.add_filter cmd "cow";
            Nbdkit.add_arg cmd "file" filename;
            if Nbdkit.version () >= (1, 22, 0) then
              Nbdkit.add_arg cmd "cache" "none";
            let _, pid = Nbdkit.run_unix socket cmd in

            (* --exit-with-parent should ensure nbdkit is cleaned
             * up when we exit, but it's not supported everywhere.
             *)
            On_exit.kill pid

         (* We use qemu-nbd for all other formats including auto-detect. *)
         | _ ->
            let cmd = QemuNBD.create filename in
            QemuNBD.set_snapshot cmd options.read_only;
            QemuNBD.set_format cmd format;
            let _, pid = QemuNBD.run_unix socket cmd in
            On_exit.kill pid
  ) disks

module Libvirt_ = struct
  let to_string options args =
    let xs = "-i libvirt" :: args in
    let xs =
      match options.input_conn with
      | Some ic -> ("-ic " ^ ic) :: xs
      | None -> xs in
    String.concat " " xs

  let query_input_options () =
    printf (f_"No input options can be used in this mode.\n")

  let setup dir options args =
    let source, data = get_source_from_libvirt options args in
    setup_servers options dir data;
    source
end

module LibvirtXML = struct
  let to_string options args = String.concat " " ("-i libvirtxml" :: args)

  let query_input_options () =
    printf (f_"No input options can be used in this mode.\n")

  let setup dir options args =
    let source, data = get_source_from_libvirt_xml options args in
    setup_servers options dir data;
    source
end
