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

let rec libvirt_source options args =
  if options.input_options <> [] then
    error (f_"no -io (input options) are allowed here");

  let guest =
    match args with
    | [arg] -> arg
    | _ ->
       error (f_"-i libvirt: expecting a libvirt guest name on the command line") in

  (* Connect to the hypervisor. *)
  let conn =
    let auth = Libvirt_utils.auth_for_password_file
                 ?password_file:options.input_password () in
    Libvirt.Connect.connect_auth ?name:options.input_conn auth in

  (* Parse the libvirt XML. *)
  let source, disks, _ = parse_libvirt_domain conn guest in
  source, disks

and libvirt_servers dir disks =
  (* Check nbdkit is installed. *)
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working.  It is required to use ‘-i libvirt|libvirtxml’.");

  if not (Nbdkit.probe_plugin "file") then
    error (f_"nbdkit-file-plugin is not installed or not working");
  if not (Nbdkit.probe_plugin "nbd") then
    error (f_"nbdkit-nbd-plugin is not installed or not working");
  if not (Nbdkit.probe_filter "cow") then
    error (f_"nbdkit-cow-filter is not installed or not working");

  let nbdkit_config = Nbdkit.config () in

  List.iteri (
    fun i { d_format = format; d_type } ->
      let socket = sprintf "%s/in%d" dir i in
      On_exit.unlink socket;

      match d_type with
      (* Forward to another NBD server using nbdkit-nbd-plugin. *)
      | NBD (hostname, port) ->
         let cmd = Nbdkit.create "nbd" in
         Nbdkit.add_filter cmd "cow";
         Nbdkit.add_arg cmd "hostname" hostname;
         Nbdkit.add_arg cmd "port" (string_of_int port);
         let _, pid = Nbdkit.run_unix ~socket cmd in

         (* --exit-with-parent should ensure nbdkit is cleaned
          * up when we exit, but it's not supported everywhere.
          *)
         On_exit.kill pid

      (* Forward to an HTTP/HTTPS server using nbdkit-curl-plugin. *)
      | HTTP url ->
         let cor = dir // "convert" in
         let cmd = Nbdkit_curl.create_curl ~cor url in
         let _, pid = Nbdkit.run_unix ~socket cmd in

         (* --exit-with-parent should ensure nbdkit is cleaned
          * up when we exit, but it's not supported everywhere.
          *)
         On_exit.kill pid

      | BlockDev filename | LocalFile filename ->
         match format with
         | Some "raw" ->
            let cmd = Nbdkit.create "file" in
            Nbdkit.add_filter cmd "cow";
            Nbdkit.add_arg cmd "file" filename;
            if Nbdkit.version nbdkit_config >= (1, 22, 0) then (
              Nbdkit.add_arg cmd "fadvise" "sequential";
              Nbdkit.add_arg cmd "cache" "none";
            );
            let _, pid = Nbdkit.run_unix ~socket cmd in

            (* --exit-with-parent should ensure nbdkit is cleaned
             * up when we exit, but it's not supported everywhere.
             *)
            On_exit.kill pid

         (* We use qemu-nbd for all other formats including auto-detect. *)
         | _ ->
            let cmd = QemuNBD.create filename in
            QemuNBD.set_snapshot cmd true; (* protective overlay *)
            QemuNBD.set_format cmd format;
            let _, pid = QemuNBD.run_unix ~socket cmd in
            On_exit.kill pid
  ) disks

and libvirt_xml_source _ args =
  let xmlfile =
    match args with
    | [arg] -> arg
    | _ ->
       error (f_"-i libvirtxml: expecting a libvirt XML filename on the command line") in
  let xml = read_whole_file xmlfile in
  let source, disks = parse_libvirt_xml xml in
  source, disks

module Libvirt_ = struct
  let setup dir options args =
    let source, data = libvirt_source options args in
    libvirt_servers dir data;
    source

  let query_input_options () =
    printf (f_"No input options can be used in this mode.\n")
end

module LibvirtXML = struct
  let setup dir options args =
    let source, data = libvirt_xml_source options args in
    libvirt_servers dir data;
    source

  let query_input_options () =
    printf (f_"No input options can be used in this mode.\n")
end
