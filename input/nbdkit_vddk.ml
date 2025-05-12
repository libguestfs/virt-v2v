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
| AskForPassword                (* password=- *)
| PasswordFile of string        (* password=+file *)

(* VDDK libraries are located under lib32/ or lib64/ relative to the
 * libdir.  Note this is unrelated to Linux multilib or multiarch.
 *)
let libNN = sprintf "lib%d" Sys.word_size

(* Create an nbdkit module specialized for reading from VDDK sources. *)
let create_vddk ?bandwidth ?config ?cookie ?cor ?libdir ~moref
      ?nfchostport ~noextents ?password_file ?port
      ~server ?snapshot ~thumbprint ?transports ?user file =
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working");

  if not (Nbdkit.probe_plugin "vddk") then
    error (f_"nbdkit-vddk-plugin is not installed");

  if not (Nbdkit.probe_filter "cow") then
    error (f_"nbdkit-cow-filter is not installed or not working");

  (* Check that the VDDK path looks reasonable. *)
  let error_unless_vddk_libdir () =
    match libdir with
    | None -> ()
    | Some libdir ->
       if not (is_directory libdir) then
         error (f_"‘-io vddk-libdir=%s’ does not point to a directory.  \
                   See the virt-v2v-input-vmware(1) manual.") libdir
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
  let cmd = Nbdkit.create "vddk" in

  (* Suppress datapath messages. *)
  Nbdkit.add_debug_flag cmd "vddk.datapath" "0";

  (* Enable VDDK stats. *)
  Nbdkit.add_debug_flag cmd "vddk.stats" "1";

  Nbdkit.add_arg cmd "server" server;
  Nbdkit.add_arg cmd "vm" (sprintf "moref=%s" moref);
  Nbdkit.add_arg cmd "file" file;

  (* For VDDK we require some user.  If it's not supplied, assume root. *)
  let user = Option.value ~default:"root" user in
  Nbdkit.add_arg cmd "user" user;

  let password =
    match password_file with
    | None -> AskForPassword
    | Some password_file -> PasswordFile password_file in

  (* The passthrough parameters. *)
  let passthru cmd name v = Option.iter (Nbdkit.add_arg cmd name) v in
  passthru cmd "config" config;
  passthru cmd "libdir" libdir;
  passthru cmd "nfchostport" nfchostport;
  passthru cmd "port" port;
  passthru cmd "snapshot" snapshot;
  Nbdkit.add_arg cmd "thumbprint" thumbprint; (* required *)
  passthru cmd "transports" transports;

  (* Retry filter (if it exists) can be used to get around brief
   * interruptions in service.  It must be closest to the plugin.
   *)
  Nbdkit.add_filter_if_available cmd "retry";

  (* VDDK's QueryAllocatedBlocks API is infamously slow.  It appears
   * to block all other requests while it is running.  This API is
   * also only called during the copy phase, not during conversion
   * (or if it is, extremely rarely).
   *
   * If fstrim was successful, then trimmed blocks are stored in
   * the COW filter (see below), and so requests for extents stop
   * at that layer.  However for areas of the disk that fstrim
   * thinks contain data, we still have to go through to VDDK to
   * fetch extents.
   *
   * We could therefore add nbdkit-noextents-filter here (below COW,
   * above VDDK plugin) which stops extents requests from going
   * to VDDK, which would stop QueryAllocatedBlocks ever being
   * called.  In my testing this is a moderate performance win.
   *
   * However ... in the case where fstrim failed, or for filesystems
   * or partitions on the disk that we don't understand, doing this
   * would mean that those are copied completely, as there would be
   * no extent data (nbdcopy will still sparsify them on the target,
   * but we'd have to copy all the bits from VMware).  Because
   * here we don't know if this is the case, be conservative and
   * actually don't use this filter.
   *
   * If used, this filter should be close to the plugin and MUST
   * be below the COW filter.
   *)
  if noextents then
    Nbdkit.add_filter_if_available cmd "noextents";

  (* Split very large requests to avoid out of memory errors on the
   * server.  Since we're using this filter, also add minblock=512
   * although it will make no difference.
   *)
  if Nbdkit.probe_filter "blocksize" then (
    Nbdkit.add_filter cmd "blocksize";
    Nbdkit.add_arg cmd "minblock" "512";
    Nbdkit.add_arg cmd "maxdata" "2M"
  );

  (* IMPORTANT! Add the COW filter.  It must be furthest away
   * except for the multi-conn and rate filters.
   *)
  Nbdkit.add_filter cmd "cow";

  (* The cow filter unconditionally enables multi-conn (because it is
   * safe).  However this causes an unintended consequence with the VDDK
   * plugin.  Multiple VDDK handles are opened (one per multi-conn
   * connection), and for some reason, possibly internal locking, they
   * conflict with each other.  This manifests itself as API calls taking
   * between 2 and 7 times longer to serve (especially QueryAllocatedBlocks
   * which seems to slow down most).
   *
   * Avoid this by adding nbdkit-multi-conn-filter with
   * multi-conn-mode=disable on top which disables multi-conn
   * advertisement.
   *)
  if Nbdkit.probe_filter "multi-conn" then (
    Nbdkit.add_filter cmd "multi-conn";
    Nbdkit.add_arg cmd "multi-conn-mode" "disable";
  );

  (* If the filter supports it, enable cow-block-size (added in
   * nbdkit 1.27.6).  This helps to reduce fragmentated small
   * extent and read requests.
   *)
  if Nbdkit.probe_filter_parameter "cow" "cow-block-size" then
    Nbdkit.add_arg cmd "cow-block-size" "4096";

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
      Nbdkit.add_arg cmd "password" ("+" ^ password_file)
  );

  cmd
