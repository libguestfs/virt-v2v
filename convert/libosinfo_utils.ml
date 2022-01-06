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

let string_of_osinfo_device_list dev_list =

  (* Turn the fields of an "osinfo_device" record into a list. *)
  let listify { Libosinfo.id; vendor; vendor_id; product; product_id; name;
                class_; bus_type; subsystem } =
    [ id; vendor; vendor_id; product; product_id; name;
      class_; bus_type; subsystem ]

  (* Given a list of strings, and a list of previously known maximum widths,
   * "increase" each width, if necessary, to the length of the corresponding
   * string.
   *)
  and grow_widths = List.map2 (fun s -> max (String.length s))
  in

  (* Compute the maximum width for each field in "dev_list". *)
  let max_widths =
    List.fold_right grow_widths (List.map listify dev_list)
      [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ]

  (* Given a list of strings and a list of field widths, format "string1 |
   * string2 | ... | stringN" such that each field is right-padded to the
   * corresponding width.
   *)
  and columnate strings widths =
    String.concat " | " (List.map2 (Printf.sprintf "%-*s") widths strings)
  in

  (* Format "dev_list" as a table by (a) printing one "osinfo_device" record
   * per line, and (b) right-padding each field of each "osinfo_device" record
   * to the maximum width of that field.
   *)
  String.concat "\n"
    (List.map (fun dev -> columnate (listify dev) max_widths) dev_list)
