(* virt-v2v
 * Copyright (C) 2020 Red Hat Inc.
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

(** This module implements helper functions based on libosinfo. *)

val get_os_by_short_id : string -> Libosinfo.osinfo_os
(** [get_os_by_short_id short-id] get the [Libosinfo.osinfo_os]
    that has the specified [short-id].

    Raise [Not_found] in case there is no matching OS.
 *)

val string_of_osinfo_device_list : Libosinfo.osinfo_device list -> string
(** Convert an [osinfo_device] list to a printable string for debugging. *)

val string_of_osinfo_device_driver : Libosinfo.osinfo_device_driver -> string
(** Convert a [osinfo_device_driver] to a printable string for debugging. *)

val best_driver : Libosinfo.osinfo_device_driver list ->
                  string ->
                  Libosinfo.osinfo_device_driver
(** [best_driver drivers arch] picks the best driver from [drivers] as follows:
    - filters out drivers that:
      - target a different architecture,
      - are not pre-installable,
      - have an invalid or non-local URL;
    - sorts the remaining drivers by priority, like libosinfo does;
    - picks the top driver of the sorted list.
    Raises Not_found if no driver in [drivers] survives filtering. *)

type os_support = {
  q35 : bool;
  vio10 : bool;
}
(** Tell whether the operating system supports the Q35 board type and/or
    non-transitional (virtio-1.0-only) virtio devices. (Internally, the
    virtio-1.0-net device is used as a proxy for the general statement about
    virtio-1.0.)
 *)

val os_support_of_osinfo_device_list : Libosinfo.osinfo_device list ->
                                       os_support
(** Get [os_support] from an [osinfo_device] list. *)
