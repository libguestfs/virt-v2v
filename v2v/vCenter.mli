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

(** Functions for dealing with VMware vCenter. *)

val qemu_uri_of_path : ?bandwidth:Types.bandwidth -> ?password_file:string ->
                       string -> Xml.uri -> string -> string -> string
(** [qemu_uri_of_path ?bandwidth ?password_file dcPath uri server path]
    maps the [<source path=...>] string to a qemu URI pointing
    to the guest disk.

    The input [path] comes from libvirt and will be something like:
    ["[datastore1] Fedora 20/Fedora 20.vmdk"]
    (including those literal spaces in the string).

    This checks that the disk exists and that authentication is
    correct, otherwise it will fail.

    The returned QEMU URI is an opaque blob (usually a [json:] URL)
    which can be passed to [qemu] or [qemu-img] as a backing
    file. *)
