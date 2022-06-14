(* virt-v2v
 * Copyright (C) 2019 Red Hat Inc.
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

(** Create JSON metadata for [-o json]. *)

val create_json_metadata : Types.source -> Types.inspect ->
                           Types.target_meta ->
                           (int -> string) -> string -> string ->
                           JSON.doc
(** [create_json_metadata source targets outdisk_map output_format output_name]
    creates the JSON with the majority of the data that virt-v2v used for the
    conversion. *)