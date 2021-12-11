(* virt-v2v
 * Copyright (C) 2009-2020 Red Hat Inc.
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

open Common_gettext.Gettext
open Std_utils
open Tools_utils

open Types
open Utils

let nbdkit_min_version = (1, 12, 0)

type password =
| NoPassword                    (* no password option at all *)
| AskForPassword                (* password=- *)
| PasswordFile of string        (* password=+file *)

let error_unless_nbdkit_version_ge config min_version =
  let version = Nbdkit.version config in
  if version < min_version then (
    let min_major, min_minor, min_release = min_version in
    error (f_"nbdkit is too old.  nbdkit >= %d.%d.%d is required.")
          min_major min_minor min_release
  )

let error_unless_nbdkit_min_version config =
  error_unless_nbdkit_version_ge config nbdkit_min_version

(* Create an nbdkit module specialized for reading from SSH sources. *)
let create_ssh ?bandwidth ?cor ~password ?port ~server ?user path =
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working");

  if not (Nbdkit.probe_plugin "ssh") then
    error (f_"nbdkit-ssh-plugin is not installed");

  if not (Nbdkit.probe_filter "cow") then
    error (f_"nbdkit-cow-filter is not installed or not working");

  let config = Nbdkit.config () in
  error_unless_nbdkit_min_version config;

  (* Construct the nbdkit command. *)
  let cmd = Nbdkit.new_cmd in
  let cmd = Nbdkit.set_plugin cmd "ssh" in

  (* Other flags. *)
  let cmd = Nbdkit.set_verbose cmd (verbose ()) in

  let cmd = Nbdkit.add_arg cmd "host" server in
  let cmd = Nbdkit.add_arg cmd "path" path in
  let cmd =
    match port with
    | Some s -> Nbdkit.add_arg cmd "port" s
    | None -> cmd in
  let cmd =
    match user with
    | Some s -> Nbdkit.add_arg cmd "user" s
    | None -> cmd in

  (* Retry filter (if it exists) can be used to get around brief
   * interruptions in service.  It must be closest to the plugin.
   *)
  let cmd = Nbdkit.add_filter_if_available cmd "retry" in

  (* Caching extents speeds up qemu-img, especially its consecutive
   * block_status requests with req_one=1.
   *)
  let cmd = Nbdkit.add_filter_if_available cmd "cacheextents" in

  (* IMPORTANT! Add the COW filter.  It must be furthest away
   * except for the rate filter.
   *)
  let cmd = Nbdkit.add_filter cmd "cow" in

  (* Add the cow-on-read flag if supported. *)
  let cmd =
    match cor with
    | None -> cmd
    | Some cor ->
       if Nbdkit.probe_filter_parameter "cow" "cow-on-read=.*/PATH" then
         Nbdkit.add_arg cmd "cow-on-read" cor
       else cmd in

  (* Add the rate filter.  This must be furthest away so that
   * we don't end up rate-limiting internal nbdkit operations.
   *)
  let cmd =
    if Nbdkit.probe_filter "rate" then (
      match bandwidth with
      | None -> cmd
      | Some bandwidth ->
         let cmd = Nbdkit.add_filter cmd "rate" in
         match bandwidth with
         | StaticBandwidth rate ->
            Nbdkit.add_arg cmd "rate" rate
         | DynamicBandwidth (None, filename) ->
            Nbdkit.add_arg cmd "rate-file" filename
         | DynamicBandwidth (Some rate, filename) ->
            Nbdkit.add_args cmd ["rate", rate; "rate-file", filename]
    )
    else cmd in

  (* Handle the password parameter specially. *)
  let cmd =
    match password with
    | NoPassword -> cmd
    | AskForPassword ->
       (* Because we will start nbdkit in the background and then wait
        * for 30 seconds for it to start up, we cannot use the
        * password=- feature of nbdkit to read the password
        * interactively (since in the words of the movie the user has
        * only "30 seconds to comply").  In any case this feature broke
        * in the VDDK plugin in nbdkit 1.18 and 1.20.  So in the
        * AskForPassword case we read the password here.
        *)
       printf "password: ";
       let open Unix in
       let orig = tcgetattr stdin in
       let tios = { orig with c_echo = false } in
       tcsetattr stdin TCSAFLUSH tios; (* Disable echo. *)
       let password = read_line () in
       tcsetattr stdin TCSAFLUSH orig; (* Restore echo. *)
       printf "\n";
       let password_file = Filename.temp_file "v2vnbdkit" ".txt" in
       On_exit.unlink password_file;
       with_open_out password_file (fun chan -> output_string chan password);
       Nbdkit.add_arg cmd "password" ("+" ^ password_file)
    | PasswordFile password_file ->
       Nbdkit.add_arg cmd "password" ("+" ^ password_file) in

  cmd

