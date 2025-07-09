(* virt-v2v
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

open Common_gettext.Gettext
open Std_utils
open Tools_utils

open Types
open Utils

type password =
  | AskForPassword
  | PasswordFile of string

(* Create an nbdkit module specialized for reading from SSH sources. *)
let create_ssh ?bandwidth ?cor ?(retry=true)
      ~server ?port ?user ?password path =
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working");

  if not (Nbdkit.probe_plugin "ssh") then
    error (f_"nbdkit-ssh-plugin is not installed");

  if not (Nbdkit.probe_filter "cow") then
    error (f_"nbdkit-cow-filter is not installed or not working");

  (* Construct the nbdkit command. *)
  let cmd = Nbdkit.create "ssh" in
  Nbdkit.add_arg cmd "host" server;
  Nbdkit.add_arg cmd "path" path;
  Option.iter (Nbdkit.add_arg cmd "port") port;
  Option.iter (Nbdkit.add_arg cmd "user") user;

  (* Retry filter (if it exists) can be used to get around brief
   * interruptions in service.  It must be closest to the plugin.
   *)
  if retry then
    Nbdkit.add_filter_if_available cmd "retry";

  (* Add the count filter if available, to report bytes read.
   * Since it writes a debug message, only do this if verbose.
   * This should be close to the plugin so we're reporting what
   * is read over the wire.
   *)
  if verbose () then Nbdkit.add_filter_if_available cmd "count";

  (* IMPORTANT! Add the COW filter.  It must be furthest away
   * except for the rate filter.
   *)
  Nbdkit.add_filter cmd "cow";

  (* Add the cow-on-read flag if supported. *)
  (match cor with
   | None -> ()
   | Some cor ->
      if Nbdkit.probe_filter_parameter "cow" "cow-on-read=.*/PATH" then
        Nbdkit.add_arg cmd "cow-on-read" cor
  );

  (* Add the rate filter.  This must be furthest away so that
   * we don't end up rate-limiting internal nbdkit operations.
   *)
  if Nbdkit.probe_filter "rate" then (
    match bandwidth with
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

  (* Handle the password parameter specially. *)
  (match password with
   | None -> ()
   | Some AskForPassword ->
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
   | Some (PasswordFile password_file) ->
      Nbdkit.add_arg cmd "password" ("+" ^ password_file)
  );

  cmd

