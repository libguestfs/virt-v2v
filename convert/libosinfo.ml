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

open Std_utils
open Tools_utils
open Common_gettext.Gettext

type osinfo_db_t
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

external osinfo_os_get_id : osinfo_os_t -> string = "v2v_osinfo_os_get_id"
external osinfo_os_get_device_drivers : osinfo_os_t -> osinfo_device_driver list = "v2v_osinfo_os_get_device_drivers"
external osinfo_os_get_devices : osinfo_os_t -> osinfo_device list = "v2v_osinfo_os_get_all_devices"

class osinfo_os h =
  object (self)
    method get_id () = osinfo_os_get_id h
    method get_device_drivers () = osinfo_os_get_device_drivers h
    method get_devices () = osinfo_os_get_devices h
end

external osinfo_db_load : unit -> osinfo_db_t = "v2v_osinfo_db_load"
external osinfo_db_find_os_by_short_id : osinfo_db_t -> string -> osinfo_os_t = "v2v_osinfo_os_find_os_by_short_id"

class osinfo_db () =
  let h = osinfo_db_load () in
  object (self)
    method find_os_by_short_id name =
      let os = osinfo_db_find_os_by_short_id h name in
      new osinfo_os os
end
