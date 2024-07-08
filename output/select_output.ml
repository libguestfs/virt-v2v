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

open Tools_utils
open Common_gettext.Gettext

type output_mode =
  | Disk
  | Kubevirt
  | Libvirt
  | Null
  | Openstack
  | QEmu

let output_modes = [
    Disk;
    Kubevirt;
    Libvirt;
    Null;
    Openstack;
    QEmu;
  ]

let string_of_output_mode = function
  | Disk -> "disk"
  | Kubevirt -> "kubevirt"
  | Libvirt -> "libvirt"
  | Null -> "null"
  | Openstack -> "openstack"
  | QEmu -> "qemu"

let output_mode_of_string = function
  | "kubevirt" -> Kubevirt
  | "libvirt" -> Libvirt
  | "disk" | "local" -> Disk
  | "null" -> Null
  | "openstack" | "osp" | "rhosp" -> Openstack
  | "qemu" -> QEmu
  | s -> error (f_"unknown -o option: %s") s

let select_output = function
  | None | Some Libvirt -> (module Output_libvirt.Libvirt_ : Output.OUTPUT)
  | Some Disk -> (module Output_disk.Disk)
  | Some Null -> (module Output_null.Null)
  | Some QEmu -> (module Output_qemu.QEMU)
  | Some Kubevirt -> (module Output_kubevirt.Kubevirt)
  | Some Openstack -> (module Output_openstack.Openstack)
