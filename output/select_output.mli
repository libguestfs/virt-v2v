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
(** [-o] option on the command line *)

val output_mode_of_string : string -> output_mode
(** Return the output mode corresponding to a string.  This is
    used when parsing the command line.  If the output mode
    is unknown this calls {!Tools_utils.error} and exits. *)

val select_output : output_mode option -> (module Output.OUTPUT)
(** Select an output module based on command line parameters. *)
