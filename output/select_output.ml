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

open Std_utils
open Tools_utils
open Common_gettext.Gettext

type output_mode =
  | Disk
  | Glance
  | Kubevirt
  | Libvirt
  | Null
  | Openstack
  | OVirt
  | OVirt_Upload
  | QEmu
  | VDSM

let output_modes =
  let modes = ref [
    Disk;
    Kubevirt;
    Libvirt;
    Null;
    Openstack;
    QEmu;
  ] in
  if Config.enable_glance then
    List.push_front Glance modes;
  if Config.enable_ovirt then
    List.push_front_list [ OVirt; OVirt_Upload; VDSM ] modes;
  List.sort compare !modes

let string_of_output_mode = function
  | Disk -> "disk"
  | Glance -> "glance"
  | Kubevirt -> "kubevirt"
  | Libvirt -> "libvirt"
  | Null -> "null"
  | Openstack -> "openstack"
  | OVirt -> "ovirt"
  | OVirt_Upload -> "ovirt-upload"
  | QEmu -> "qemu"
  | VDSM -> "vdsm"

let output_mode_of_string = function
  | "glance" -> Glance
  | "kubevirt" -> Kubevirt
  | "libvirt" -> Libvirt
  | "disk" | "local" -> Disk
  | "null" -> Null
  | "openstack" | "osp" | "rhosp" -> Openstack
  | "ovirt" | "rhv" | "rhev" -> OVirt
  | "ovirt-upload" | "ovirt_upload" | "rhv-upload" | "rhv_upload" ->
     OVirt_Upload
  | "qemu" -> QEmu
  | "vdsm" -> VDSM
  | s -> error (f_"unknown -o option: %s") s

let select_output = function
  | None | Some Libvirt -> (module Output_libvirt.Libvirt_ : Output.OUTPUT)
  | Some Disk -> (module Output_disk.Disk)
  | Some Null -> (module Output_null.Null)
  | Some QEmu -> (module Output_qemu.QEMU)
  | Some Glance ->
     if Config.enable_glance then (module Output_glance.Glance)
     else failwithf "-o glance is not enabled in this build"
  | Some Kubevirt -> (module Output_kubevirt.Kubevirt)
  | Some Openstack -> (module Output_openstack.Openstack)
  | Some OVirt_Upload ->
     if Config.enable_ovirt then (module Output_ovirt_upload.OVirtUpload)
     else failwithf "-o ovirt-upload is not enabled in this build"
  | Some OVirt ->
     if Config.enable_ovirt then (module Output_ovirt.OVirt)
     else failwithf "-o ovirt is not enabled in this build"
  | Some VDSM ->
     if Config.enable_ovirt then (module Output_vdsm.VDSM)
     else failwithf "-o vdsm is not enabled in this build"
