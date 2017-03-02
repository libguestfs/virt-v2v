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

    let nr_disks = List.length args in
    if nr_disks = 0 then
      error (f_"-i disk: expecting a disk image (filename) \
                on the command line");

    (* Check nbdkit is installed. *)
    if not (Nbdkit.is_installed ()) then
      error (f_"nbdkit is not installed or not working.  It is required to \
                use ‘-i disk’.");
    if not (Nbdkit.probe_plugin "file") then
      error (f_"nbdkit-file-plugin is not installed or not working");
    if options.read_only && not (Nbdkit.probe_filter "cow") then
      error (f_"nbdkit-cow-filter is not installed or not working");

    (* Each disk on the command line can be a local file or an NBD URI. *)
    let is_nbd_uri =
      let h = lazy (NBD.create ()) in
      fun str -> NBD.is_uri (Lazy.force h) str
    in

    (* Function for detecting input format, using guestfs_disk_format. *)
    let detect_input_format =
      let g = lazy (open_guestfs ()) in
      fun disk -> (Lazy.force g)#disk_format disk
    in

    (* If there are any NBD URIs on the command line, nbdkit-nbd-plugin
     * must be available too.
     *)
    if List.exists is_nbd_uri args && not (Nbdkit.probe_plugin "nbd") then
      error (f_"nbdkit-nbd-plugin is not installed or not working");

    (* What name should we use for the guest?  We try to derive it from
     * the first filename passed in.  If there are no filenames then
     * we use "unknown".
     *
     * Users can override this using the '-on name' option.
     *)
    let name =
      let rec loop = function
        | [] ->
(* XXX can't print this because we don't know if the user set '-on'
           warning (f_"-i disk: no local filenames given so we set \
                       the guest name to \"unknown\".  Use ‘-on name’ to \
                       override the guest name.");
*)
           "unknown"
        | x :: xs when is_nbd_uri x ->
           loop xs
        | x :: _ ->
           name_from_disk x
      in
      loop args in

    (* Convert the command line disks to NBD URIs. *)
    let uris =
      List.mapi (
        fun i disk ->
          let sockname = sprintf "in%d" i in
          let socket = sprintf "%s/%s" dir sockname in
          On_exit.unlink socket;

          if is_nbd_uri disk then (
            (* For nbd:// on the command line, proxy through
             * nbdkit-nbd-plugin.  This is not ideal.  We could
             * catch special cases here, such as nbd+unix://
             * and convert them directly to NBD_URI objects.
             * That would require parsing NBD URIs ourselves
             * but would avoid needing an extra nbdkit proxy.
             * XXX
             *)
            let cmd = Nbdkit.create ~name:sockname "nbd" in
            if options.read_only then
              Nbdkit.add_filter cmd "cow";
            Nbdkit.add_arg cmd "uri" disk;
            let _, pid = Nbdkit.run_unix socket cmd in

            (* --exit-with-parent should ensure nbdkit is cleaned
             * up when we exit, but it's not supported everywhere.
             *)
            On_exit.kill pid
          )
          else (
            (* Local filename.  Use nbdkit-file-plugin for raw,
             * or qemu-nbd for other formats.
             *
             * virt-v2v < 2.12 used to enforce that all disks
             * had the same format, but that seems unnecessary.
             *)

            (* Check the input file exists and is readable. *)
            access disk [R_OK];

            let format =
              match options.input_format with
              | Some fmt -> fmt (* -if option overrides everything *)
              | None -> detect_input_format disk in

            match format with
            | "raw" ->
               let cmd = Nbdkit.create ~name:sockname "file" in
               if options.read_only then
                 Nbdkit.add_filter cmd "cow";
               Nbdkit.add_arg cmd "file" disk;
               Nbdkit.reduce_memory_pressure cmd;
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

    let s_disks =
      List.make nr_disks () |>
      List.mapi (
        fun i () -> { s_disk_id = i; s_controller = None }
      ) in

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

    source, uris
end
