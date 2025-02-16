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

(** Parse OVF from an externally produced OVA file.

    This is used by [-i ova] only.  OVA files are not a real standard
    so we must make some assumptions here. *)

type disk = {
  source_disk : Types.source_disk;
  href : string;                (** The <File href> from the OVF file. *)
  compressed : bool;            (** If the href is gzip compressed. *)
}
(** A VMDK disk from a parsed OVF. *)

val parse_ovf_from_ova : string -> string option * int64 * int * Types.source_cpu_topology option * Types.source_firmware * disk list * Types.source_removable list * Types.source_nic list
(** Parse an OVF file.

    The returned tuple is
    [name, memory, vcpu, cpu_topology, firmware,
    disks, removables, nics] *)

val parse_disks : string -> disk list
(** As above, but returns only the disks. *)
