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
open Unix_utils
open Common_gettext.Gettext

open Printf

let start_nbdkit ~password ?port ~server ?user path =
  (* Create a random location for the socket used to talk to nbdkit. *)
  let sockdir = Mkdtemp.temp_dir "v2vssh." in
  On_exit.rm_rf sockdir;
  let id = unique () in
  let socket = sockdir // sprintf "nbdkit%d.sock" id in

  (* Note: Disabling the retry filter helps in the missing file case,
   * otherwise nbdkit takes ages to time out.  We're not expecting that
   * the VMX file is large, so using this filter isn't necessary.
   *)
  let nbdkit =
    Nbdkit_ssh.create_ssh ~retry:false ~password ~server ?port ?user path in
  Nbdkit.set_readonly nbdkit true;
  let _, pid = Nbdkit.run_unix socket nbdkit in
  On_exit.kill pid;

  (* Return the URI of nbdkit. *)
  "nbd+unix://?socket=" ^ socket

(* Download a remote file into a local file. *)
let download_file ~password ?port ~server ?user path output =
  let uri = start_nbdkit ~password ?port ~server ?user path in

  let cmd = [ "nbdcopy"; uri; output ] in
  if run_command cmd <> 0 then
    error (f_"could not copy the VMX file from the remote server, \
              see earlier error messages")

(* Test if [path] exists on the remote server. *)
let remote_file_exists ~password ?port ~server ?user path =
  let uri = start_nbdkit ~password ?port ~server ?user path in

  (* Testing for remote size using nbdinfo should be sufficient to
   * prove the remote file exists.
   *)
  let cmd = [ "nbdinfo"; "--size"; uri ] in
  run_command cmd = 0
