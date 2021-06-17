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

(** Parse libvirt XML into a {!Types.source} structure and list of disks. *)

type disk = {
  d_format : string option;     (** Disk format from XML if known. *)
  d_type : disk_type;           (** Disk type and extra information. *)
}
and disk_type =
  | BlockDev of string          (** type=block with <source dev=...> *)
  | LocalFile of string         (** type=file with <source file=...> *)
  | NBD of string * int         (** NBD forward to hostname:port *)
  | HTTP of string              (** HTTP/HTTPS URL *)
(** Libvirt disk description corresponding to each field in s_disks.
    The caller usually has to create NBD server instances for each
    of these. *)

val parse_libvirt_domain : Libvirt.rw Libvirt.Connect.t -> string -> Types.source * disk list * string
(** [parse_libvirt_domain conn dom] loads the XML of the domain [dom]
    from the libvirt connection [conn].
    The result is a tuple with a {!Types.source} structure, a list of
    libvirt disks, and the XML of the guest. *)

val parse_libvirt_xml : ?conn:Libvirt.rw Libvirt.Connect.t -> string -> Types.source * disk list
(** Take libvirt XML and parse it into a {!Types.source} structure and a
    list of libvirt disks. *)
