(* virt-v2v
 * Copyright (C) 2009-2022 Red Hat Inc.
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

open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Types
open Utils
open YAML

let create_kubevirt_yaml source inspect
      { guestcaps; target_buses; target_firmware; target_nics }
      outdisk_name output_format output_name =
  (* The body of the YAML contains various sections attached to
   * a tree.  We fill in these sections first.
   *)
  let metadata = ref [] in
  let devices = ref [] in
  let disks = ref [] in
  let resources = ref [] in
  let volumes = ref [] in

  (* The guest name. *)
  List.push_back metadata ("name", String output_name);

  (* The one Windows example I have includes this clock section, and
   * the other non-Windows examples do not.  I'm not certain this
   * is correct. XXX
   *)
  if inspect.i_type = "windows" then (
    List.push_back resources (
      "clock", Assoc [
        "timer", Assoc [
          "hpet", Assoc [ "present", Bool false ];
          "hyperv", List [];
          "pit", Assoc [ "tickPolicy", String "delay" ];
          "rtc", Assoc [ "tickPolicy", String "catchup" ];
        ];
        (* XXX Note that we may need to set "localtime" here
         * depending on guestcaps.gcaps_rtc_utc.  However that
         * requires the following PR to be merged in Kubevirt:
         * https://github.com/kubevirt/kubevirt/pull/9587
         *)
        "utc", List []
      ]
    )
  );

  (* XXX genid *)

  (* Memory. *)
  let memory_str = sprintf "%LdMi" (source.s_memory /^ 1024_L /^ 1024_L) in
  List.push_back resources ("requests", Assoc ["memory", String memory_str]);

  (* Machine features. *)
  let features = List.map (fun name -> name, List []) source.s_features in
  List.push_back resources ("features", Assoc features);

  (* # vCPUs. XXX vendor, model, topology *)
  let cpu = "cpu", Assoc ["cores", Int source.s_vcpu] in

  (* XXX firmware, display, sound *)

  (* XXX guestcaps: rng, balloon, vsock, virtio 1.0 *)

  (* We're using local ("host") disks here which is not realistic. *)
  Array.iter (
    function
    | BusSlotEmpty ->
       (* XXX How to place devices on the bus? *) ()
    | BusSlotDisk d ->
       let disk_id = sprintf "disk-%d" d.s_disk_id in
       let disk = Assoc [
         "disk", Assoc ["bus", String "virtio"];
         "name", String disk_id
       ] in
       List.push_back disks disk;
       let vol = Assoc [
         "hostDisk", Assoc [
           "path", String (outdisk_name d.s_disk_id);
           "type", String "Disk";
         ];
         "name", String disk_id
       ] in
       List.push_back volumes vol
    | BusSlotRemovable _ ->
       (* XXX removables *) ()
  ) target_buses.target_virtio_blk_bus;

  (* XXX ide, scsi, floppy, NICs *)

  (* Create the final document. *)
  if !disks <> [] then
    List.push_back devices ("disks", List !disks);
  let domain = ref [] in
  if !devices <> [] then
    List.push_back domain ("devices", Assoc !devices);
  List.push_back domain ("resources", Assoc !resources);
  List.push_back domain cpu;

  let spec = ref [] in
  List.push_back spec ("domain", Assoc !domain);
  if !volumes <> [] then
    List.push_back spec ("volumes", List !volumes);
  List.push_back spec ("terminationGracePeriodSeconds", Int 0);

  let body = [
    "#", String generated_by;
    "apiVersion", String "kubevirt.io/v1";
    "kind", String "VirtualMachineInstance";
    "metadata", Assoc !metadata;
    "spec", Assoc !spec;
  ] in

  (* Return the YAML document. *)
  Doc (Assoc body)
