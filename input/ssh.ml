(* virt-v2v
 * Copyright (C) 2009-2024 Red Hat Inc.
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

open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Printf

(* Return various fields from the URI.  The checks in vmx_source_of_arg
 * should ensure that none of these assertions fail.
 *)
let port_of_uri { Xml.uri_port } =
  match uri_port with i when i <= 0 -> None | i -> Some i
let server_of_uri { Xml.uri_server } =
  match uri_server with None -> assert false | Some s -> s
let path_of_uri { Xml.uri_path } =
  match uri_path with None -> assert false | Some p -> p

(* 'scp' a remote file into a temporary local file, returning the path
 * of the temporary local file.
 *)
let scp_from_remote_to_temporary uri tmpdir filename =
  let localfile = tmpdir // filename in

  let cmd =
    sprintf "scp%s%s %s%s:%s %s"
            (if verbose () then "" else " -q")
            (match port_of_uri uri with
             | None -> ""
             | Some port -> sprintf " -P %d" port)
            (match uri.Xml.uri_user with
             | None -> ""
             | Some user -> quote user ^ "@")
            (quote (server_of_uri uri))
            (quote (path_of_uri uri))
            (quote localfile) in
  if verbose () then
    eprintf "%s\n%!" cmd;
  if Sys.command cmd <> 0 then
    error (f_"could not copy the VMX file from the remote server, \
              see earlier error messages");
  localfile

(* Test if [path] exists on the remote server. *)
let remote_file_exists uri path =
  let cmd =
    sprintf "ssh%s %s%s test -f %s"
            (match port_of_uri uri with
             | None -> ""
             | Some port -> sprintf " -p %d" port)
            (match uri.Xml.uri_user with
             | None -> ""
             | Some user -> quote user ^ "@")
            (quote (server_of_uri uri))
            (* Double quoting is necessary for 'ssh', first to protect
             * from the local shell, second to protect from the remote
             * shell.  https://github.com/libguestfs/virt-v2v/issues/35#issuecomment-1741730963
             *)
            (quote (quote path)) in
  if verbose () then
    eprintf "%s\n%!" cmd;
  Sys.command cmd = 0
