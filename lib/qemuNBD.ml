(* virt-v2v
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

(* qemu-nbd as an abstract data type. *)

open Unix
open Printf

open Common_gettext.Gettext
open Std_utils
open Tools_utils
open Unix_utils

open Utils

let is_installed =
  let test = lazy (Sys.command "qemu-nbd --version >/dev/null 2>&1" = 0) in
  fun () -> Lazy.force test

let qemu_nbd_has_selinux_label_option =
  let test = lazy (Sys.command "qemu-nbd --help |& grep -sq selinux" = 0) in
  fun () -> Lazy.force test

type version = int * int * int

let version =
  let rex = PCRE.compile "(\\d+)\\.(\\d+)\\.(\\d+)" in
  fun config ->
    let lines = external_command "qemu-nbd --version" in
    let line = List.hd lines in
    if not (PCRE.matches rex line) then
      error (f_"qemu-nbd: unexpected version in --version: %s") line;
    let major = int_of_string (PCRE.sub 1)
    and minor = int_of_string (PCRE.sub 2)
    and release = int_of_string (PCRE.sub 3) in
    debug "qemu-nbd version: %d.%d.%d" major minor release;
    (major, minor, release)

type cmd = {
  disk : string;
  mutable snapshot : bool;
  mutable format : string option;
  mutable imgopts : bool;
}

let create disk = { disk; snapshot = false; format = None; imgopts = false }

let set_snapshot cmd snap = cmd.snapshot <- snap
let set_format cmd format = cmd.format <- format
let set_image_opts cmd imgopts = cmd.imgopts <- imgopts

let run_unix socket { disk; snapshot; format; imgopts } =
  assert (disk <> "");

  (* Create a temporary directory where we place the PID file. *)
  let piddir = Mkdtemp.temp_dir "v2vqemunbd." in
  On_exit.rm_rf piddir;

  let id = unique () in
  let pidfile = piddir // sprintf "qemunbd%d.pid" id in

  (* Construct the qemu-nbd command line. *)
  let args = ref [] in
  List.push_back_list args
    ["qemu-nbd";
     "-t";
     "--shared=0";
     "--discard=unmap";
     "--pid-file"; pidfile;
     "--socket"; socket];

  (* -s adds a protective overlay. *)
  if snapshot then List.push_back args "-s";

  (* --image-opts reinterprets the filename parameter as a set of
   * image options.
   *)
  if imgopts then List.push_back args "--image-opts";

  if have_selinux && qemu_nbd_has_selinux_label_option () then (
    List.push_back args "--selinux-label";
    List.push_back args "system_u:object_r:svirt_socket_t:s0"
  );

  Option.may (
    fun format ->
      List.push_back args "--format";
      List.push_back args format
  ) format;

  List.push_back args disk;

  (* Print the full command we are about to run when debugging. *)
  if verbose () then (
    eprintf "running qemu-nbd:\n";
    List.iter (fun arg -> eprintf " %s" (quote arg)) !args;
    prerr_newline ()
  );

  let args = Array.of_list !args in
  let pid = fork () in
  if pid = 0 then (
    (* Child process. *)
    execvp "qemu-nbd" args
  );

  (* Wait for qemu-nbd to write a PID file. *)
  if not (wait_for_file pidfile 30) then (
    if verbose () then
      error (f_"qemu-nbd did not start up.  See previous debugging messages for problems.")
    else
      error (f_"qemu-nbd did not start up.  There may be errors printed by qemu-nbd above.

If the messages above are not sufficient to diagnose the problem then add the ‘virt-v2v -v -x’ options and examine the debugging output carefully.")
  );

  if have_selinux then (
    (* Note that Unix domain sockets have both a file label and
     * a socket/process label.  Using --selinux-label above
     * only set the socket label, but we must also set the file
     * label.
     *)
    ignore (run_command ["chcon"; "system_u:object_r:svirt_image_t:s0";
                         socket]);
  );

  (* Set the regular Unix permissions, in case qemu is
   * running as another user.
   *)
  chown_for_libvirt_rhbz_1045069 socket;
  chmod socket 0o700;

  (* We don't need the PID file any longer. *)
  unlink pidfile;

  socket, pid
