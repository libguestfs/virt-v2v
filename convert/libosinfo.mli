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

(** This module implements a minimal libosinfo API. *)

type osinfo_os_t

type osinfo_device = {
  id : string;
  vendor : string;
  vendor_id : string;
  product : string;
  product_id : string;
  name : string;
  class_ : string;
  bus_type : string;
  subsystem : string;
}

type osinfo_device_driver = {
  architecture : string;
  location : string;
  pre_installable : bool;
  signed : bool;
  priority : int64;
  files : string list;
  devices : osinfo_device list;
}

class osinfo_os : osinfo_os_t -> object
  method get_id : unit -> string
  (** Return the ID. *)
  method get_device_drivers : unit -> osinfo_device_driver list
  (** Return the list of device drivers. *)
  method get_devices : unit -> osinfo_device list
  (** Return the list of devices. *)
end
(** Minimal OsinfoOs wrapper. *)

class osinfo_db : unit -> object
  method find_os_by_short_id : string -> osinfo_os
  (** [find_os_by_short_id short-id] get the [osinfo_os] that has the
      specified [short-id].

      Raise [Not_found] in case there is no matching OS.
   *)
end
(** Minimal OsinfoDb wrapper. *)
