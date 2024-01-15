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

open Parse_domain_from_vmx
open Input

module VMX = struct
  let to_string options args = String.concat " " ("-i vmx" :: args)

  let query_input_options () =
    printf (f_"No input options can be used in this mode.\n")

  let rec setup dir options args =
    if options.input_options <> [] then
      error (f_"no -io (input options) are allowed here");

    if not options.read_only then
      error (f_"in-place mode does not work with VMX source");

    let vmx_source =
      match args with
      | [arg] ->
         let input_password =
           match options.input_password with
           | None -> Nbdkit_ssh.NoPassword
           | Some ip -> Nbdkit_ssh.PasswordFile ip in
         let input_transport =
           match options.input_transport with
           | None -> None
           | Some `SSH -> Some `SSH
           | Some `VDDK ->
              error (f_"-i vmx: cannot use -it vddk in this input mode") in
         vmx_source_of_arg input_password input_transport arg
      | _ ->
         error (f_"-i vmx: expecting a VMX file or ssh:// URI") in

    let source, filenames = parse_domain_from_vmx vmx_source in

    (match vmx_source with
     | File vmx_filename ->
        (* Local file in VMDK format, use qemu-nbd. *)
        List.iteri (
          fun i filename ->
            let socket = sprintf "%s/in%d" dir i in
            On_exit.unlink socket;

            let cmd = QemuNBD.create
                        (absolute_path_from_other_file vmx_filename filename) in
            QemuNBD.set_snapshot cmd true; (* protective overlay *)
            QemuNBD.set_format cmd (Some "vmdk");
            let _, pid = QemuNBD.run_unix socket cmd in
            On_exit.kill pid
        ) filenames

     | SSH (password, uri) ->
        List.iteri (
          fun i filename ->
            let socket = sprintf "%s/in%d" dir i in
            On_exit.unlink socket;

            let vmx_path = Ssh.path_of_uri uri in
            let abs_path = absolute_path_from_other_file vmx_path filename in
            let flat_vmdk = PCRE.replace (PCRE.compile "\\.vmdk$")
                              "-flat.vmdk" abs_path in

            (* RHBZ#1774386 *)
            if not (Ssh.remote_file_exists uri flat_vmdk) then
              error (f_"This transport does not support guests with snapshots. \
                        Either collapse the snapshots for this guest and try \
                        the conversion again, or use one of the alternate \
                        conversion methods described in \
                        virt-v2v-input-vmware(1) section \"NOTES\".");

            let server = Ssh.server_of_uri uri in
            let port = Option.map string_of_int (Ssh.port_of_uri uri) in
            let user = uri.Xml.uri_user in
            let cor = dir // "convert" in
            let bandwidth = options.bandwidth in
            let nbdkit = Nbdkit_ssh.create_ssh ?bandwidth ~cor ~password
                           ~server ?port ?user flat_vmdk in
            let _, pid = Nbdkit.run_unix socket nbdkit in
            On_exit.kill pid
        ) filenames
    );

    source

  (* The filename can be an absolute path, but is more often a
   * path relative to the location of the vmx file (which might
   * be remote over SSH).
   *)
  and absolute_path_from_other_file other_filename filename =
    if not (Filename.is_relative filename) then filename
    else (Filename.dirname (absolute_path other_filename)) // filename

end
