(* helper-v2v-input
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
open Unix

open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Types
open Utils

open Input

module Disk = struct
  let to_string options args = String.concat " " ("-i disk" :: args)

  let query_input_options () =
    printf (f_"No input options can be used in this mode.\n")

  let rec setup dir options args =
    if options.input_options <> [] then
      error (f_"no -io (input options) are allowed here");

    if args = [] then
      error (f_"-i disk: expecting a disk image (filename) \
                on the command line");

    (* Check the input files exist and are readable. *)
    List.iter (fun disk -> access disk [R_OK]) args;

    (* What name should we use for the guest?  We try to derive it from
     * the first filename passed in.  Users can override this using the
     * `-on name' option.
     *)
    let name = name_from_disk (List.hd args) in

    let s_disks =
      List.mapi (
        fun i _ -> { s_disk_id = i; s_controller = None }
      ) args in

    (* Give the guest a simple generic network interface. *)
    let s_nic = {
      s_mac = None;
      s_nic_model = None;
      s_vnet = "default";
      s_vnet_type = Network;
    } in

    let source = {
      s_hypervisor = UnknownHV;
      s_name = name;
      s_genid = None;
      s_memory = 2048L *^ 1024L *^ 1024L; (* 2048 MB *)
      s_vcpu = 1;            (* 1 vCPU is a safe default *)
      s_cpu_vendor = None;
      s_cpu_model = None;
      s_cpu_topology = None;
      s_features = [ "acpi"; "apic"; "pae" ];
      s_firmware = UnknownFirmware; (* causes virt-v2v to autodetect *)
      s_uefi_secureboot = false;
      s_display =
        Some { s_display_type = VNC; s_keymap = None; s_password = None;
               s_listen = LNoListen; s_port = None };
      s_sound = None;
      s_disks = s_disks;
      s_removables = [];
      s_nics = [s_nic];
    } in

    let input_format = detect_local_input_format options args in

    (* Check nbdkit is installed. *)
    if not (Nbdkit.is_installed ()) then
      error (f_"nbdkit is not installed or not working.  It is required to \
                use ‘-i disk’.");

    if not (Nbdkit.probe_plugin "file") then
      error (f_"nbdkit-file-plugin is not installed or not working");
    if options.read_only && not (Nbdkit.probe_filter "cow") then
      error (f_"nbdkit-cow-filter is not installed or not working");

    let uris =
      List.mapi (
        fun i disk ->
          let sockname = sprintf "in%d" i in
          let socket = sprintf "%s/%s" dir sockname in
          On_exit.unlink socket;

          (match input_format with
           | "raw" ->
              let cmd = Nbdkit.create ~name:sockname "file" in
              if options.read_only then
                Nbdkit.add_filter cmd "cow";
              Nbdkit.add_arg cmd "file" disk;
              Nbdkit.add_arg cmd "cache" "none";
              let _, pid = Nbdkit.run_unix socket cmd in

              (* --exit-with-parent should ensure nbdkit is cleaned
               * up when we exit, but it's not supported everywhere.
               *)
              On_exit.kill pid

           | format ->
              let cmd = QemuNBD.create disk in
              QemuNBD.set_snapshot cmd options.read_only;
              QemuNBD.set_format cmd (Some format);
              let _, pid = QemuNBD.run_unix socket cmd in
              On_exit.kill pid
          );

          NBD_URI.Unix (socket, None)
      ) args in

    source, uris

  (* For a list of local disks, try to detect the input format if
   * the [-if] option was not used on the command line.  If the
   * formats of the disks are different, that is an error.
   *)
  and detect_local_input_format { input_format } filenames =
    match input_format with
    | Some fmt -> fmt
    | None ->
       let formats =
         let g = open_guestfs () in
         List.map g#disk_format filenames in

       let rec get_format = function
         | [] -> error (f_"expected >= 1 disk name on the command line")
         | [x] -> x
         | x :: y :: xs when compare x y = 0 -> get_format (y :: xs)
         | _ -> error (f_"disks on the command line have mixed formats")
       in

       get_format formats
end
