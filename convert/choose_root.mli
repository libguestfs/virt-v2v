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

(** Choose which root device to convert.

    This handles the [--root] command line option. *)

val choose_root : Types.root_choice -> Guestfs.guestfs -> string
(** Do libguestfs inspection on the guest.

    Before calling this, the disks must be added to the handle
    and the handle must be launched.

    Depending on the contents of [root_choice] (the [--root] command
    line option) choose which root device to convert.  A single
    root device is returned.

    Note that this function may be interactive ([--root ask]). *)
