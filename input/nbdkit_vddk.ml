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

(* This is the first version that did not require LD_LIBRARY_PATH. *)
let nbdkit_min_version = (1, 17, 10)

type password =
| AskForPassword                (* password=- *)
| PasswordFile of string        (* password=+file *)

(* Check that nbdkit is available and new enough. *)
let error_unless_nbdkit_version_ge config min_version =
  let version = Nbdkit.version config in
  if version < min_version then (
    let min_major, min_minor, min_release = min_version in
    error (f_"nbdkit is too old.  nbdkit >= %d.%d.%d is required.")
          min_major min_minor min_release
  )

let error_unless_nbdkit_min_version config =
  error_unless_nbdkit_version_ge config nbdkit_min_version

(* VDDK libraries are located under lib32/ or lib64/ relative to the
 * libdir.  Note this is unrelated to Linux multilib or multiarch.
 *)
let libNN = sprintf "lib%d" Sys.word_size

(* Create an nbdkit module specialized for reading from VDDK sources. *)
let create_vddk ?bandwidth ?config ?cookie ?cor ?libdir ~moref
                ?nfchostport ?password_file ?port
                ~server ?snapshot ~thumbprint ?transports ?user path =
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working");

  if not (Nbdkit.probe_plugin "vddk") then
    error (f_"nbdkit-vddk-plugin is not installed");

  if not (Nbdkit.probe_filter "cow") then
    error (f_"nbdkit-cow-filter is not installed or not working");

  error_unless_nbdkit_min_version (Nbdkit.config ());

  (* Check that the VDDK path looks reasonable. *)
  let error_unless_vddk_libdir () =
    match libdir with
    | None -> ()
    | Some libdir ->
       if not (is_directory libdir) then
         error (f_"‘-io vddk-libdir=%s’ does not point to a directory.  See the virt-v2v-input-vmware(1) manual.") libdir
  in

  (* Check that the VDDK plugin is installed and working.  We also
   * check this later when calling common_create, but this version
   * has better troubleshooting output.
   *)
  let error_unless_nbdkit_vddk_working () =
    let cmd = "nbdkit vddk --dump-plugin >/dev/null" in
    if Sys.command cmd <> 0 then (
      (* See if we can diagnose why ... *)
      let cmd = "LANG=C nbdkit vddk --dump-plugin 2>&1 |
                     grep -sq \"cannot open shared object file\"" in
      let needs_library = Sys.command cmd = 0 in
      if not needs_library then
        error (f_"nbdkit VDDK plugin is not installed or not working.  It is required if you want to use VDDK.

The VDDK plugin is not enabled by default when you compile nbdkit.  You have to read the instructions in the nbdkit sources under ‘plugins/vddk/README.VDDK’ to find out how to enable the VDDK plugin.

See also the virt-v2v-input-vmware(1) manual.")
      else
        error (f_"nbdkit VDDK plugin is not installed or not working.  It is required if you want to use VDDK.

It looks like you did not set the right path in the ‘-io vddk-libdir’ option, or your copy of the VDDK directory is incomplete.  There should be a library called ’<libdir>/%s/libvixDiskLib.so.?’.

See also the virt-v2v-input-vmware(1) manual.") libNN
    )
  in

  error_unless_vddk_libdir ();
  error_unless_nbdkit_vddk_working ();

  (* Construct the nbdkit command. *)
  let cmd = Nbdkit.new_cmd in
  let cmd = Nbdkit.set_plugin cmd "vddk" in

  (* Environment.  We always add LANG=C. *)
  let cmd = Nbdkit.add_env cmd "LANG" "C" in

  (* Suppress datapath messages. *)
  let cmd = Nbdkit.add_debug_flag cmd "vddk.datapath" "0" in

  (* Enable VDDK stats. *)
  let cmd = Nbdkit.add_debug_flag cmd "vddk.stats" "1" in

  (* Other flags. *)
  let cmd = Nbdkit.set_verbose cmd (verbose ()) in
  let cmd = Nbdkit.set_exportname cmd "/" in

  (* For VDDK we require some user.  If it's not supplied, assume root. *)
  let user = Option.default "root" user in

  let cmd = Nbdkit.add_arg cmd "server" server in
  let cmd = Nbdkit.add_arg cmd "user" user in
  let cmd = Nbdkit.add_arg cmd "vm" (sprintf "moref=%s" moref) in
  let cmd = Nbdkit.add_arg cmd "file" path in

  let password =
    match password_file with
    | None -> AskForPassword
    | Some password_file -> PasswordFile password_file in

  (* The passthrough parameters. *)
  let passthru cmd name v =
    match v with
    | Some s -> Nbdkit.add_arg cmd name s
    | None -> cmd
  in
  let cmd = passthru cmd "config" config in
  let cmd = passthru cmd "libdir" libdir in
  let cmd = passthru cmd "nfchostport" nfchostport in
  let cmd = passthru cmd "port" port in
  let cmd = passthru cmd "snapshot" snapshot in
  let cmd = Nbdkit.add_arg cmd "thumbprint" thumbprint in
  let cmd = passthru cmd "transports" transports in

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

  (* If the filter supports it, enable cow-block-size (added in
   * nbdkit 1.27.6).  This helps to reduce fragmentated small
   * extent and read requests.
   *)
  let cmd =
    if Nbdkit.probe_filter_parameter "cow" "cow-block-size" then
      Nbdkit.add_arg cmd "cow-block-size" "1M"
    else cmd in

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
       unlink_on_exit password_file;
       with_open_out password_file (fun chan -> output_string chan password);
       Nbdkit.add_arg cmd "password" ("+" ^ password_file)
    | PasswordFile password_file ->
       Nbdkit.add_arg cmd "password" ("+" ^ password_file) in
  cmd
