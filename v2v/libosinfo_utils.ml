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

(* Singleton DB, created on the first access. *)
let db = lazy (new Libosinfo.osinfo_db ())
(*
 * Helper function to get the DB -- use it as sole way to get the DB.
 *)
let get_db () =
  Lazy.force db

let get_os_by_short_id os =
  let os = (get_db ())#find_os_by_short_id os in
  debug "libosinfo: loaded OS: %s" (os#get_id ());
  os

let string_of_osinfo_device_driver { Libosinfo.architecture; location;
                                     pre_installable; signed; priority;
                                     files } =
  Printf.sprintf "%s: [%s, %s, %s, priority %Ld] %s"
    location architecture
    (if pre_installable then "pre-installable" else "not pre-installable")
    (if signed then "signed" else "unsigned")
    priority
    (String.concat " " files)
