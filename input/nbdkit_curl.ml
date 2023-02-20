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

let error_unless_nbdkit_version_ge config min_version =
  let version = Nbdkit.version config in
  if version < min_version then (
    let min_major, min_minor, min_release = min_version in
    error (f_"nbdkit is too old.  nbdkit >= %d.%d.%d is required.")
          min_major min_minor min_release
  )

(* Create an nbdkit module specialized for reading from Curl sources. *)
let create_curl ?bandwidth ?cookie_script ?cookie_script_renew ?cor
                ?(sslverify=true) url =
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working");

  if not (Nbdkit.probe_plugin "curl") then
    error (f_"nbdkit-curl-plugin is not installed");

  if not (Nbdkit.probe_filter "cow") then
    error (f_"nbdkit-cow-filter is not installed or not working");

  (* The cookie* parameters require nbdkit 1.22, so check that early. *)
  if cookie_script <> None || cookie_script_renew <> None then (
    let config = Nbdkit.config () in
    error_unless_nbdkit_version_ge config (1, 22, 0)
  );

  (* Construct the nbdkit command. *)
  let cmd = Nbdkit.create "curl" in
  Nbdkit.add_arg cmd "url" url;

  (* https://bugzilla.redhat.com/show_bug.cgi?id=1146007#c10 *)
  Nbdkit.add_arg cmd "timeout" "2000";
  Option.iter (Nbdkit.add_arg cmd "cookie-script") cookie_script;
  (match cookie_script_renew with
   | Some i -> Nbdkit.add_arg cmd "cookie-script-renew" (string_of_int i)
   | None -> ());
  if not sslverify then Nbdkit.add_arg cmd "sslverify" "false";

  (* For lots of extra debugging, uncomment one or both lines. *)
  (* Nbdkit.add_arg cmd "--debug" "curl.verbose=1"; *)
  (* Nbdkit.add_arg cmd "--debug" "curl.scripts=1"; *)

  (* Retry filter (if it exists) can be used to get around brief
   * interruptions in service.  It must be closest to the plugin.
   *)
  Nbdkit.add_filter_if_available cmd "retry";

  (* Caching extents speeds up qemu-img, especially its consecutive
   * block_status requests with req_one=1.
   *)
  Nbdkit.add_filter_if_available cmd "cacheextents";

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

  cmd
