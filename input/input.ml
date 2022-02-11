(* helper-v2v-input
 * Copyright (C) 2009-2021 Red Hat Inc.
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

type options = {
  bandwidth : Types.bandwidth option;
  input_conn : string option;
  input_format : string option;
  input_options : (string * string) list;
  input_password : string option;
  input_transport : [`SSH|`VDDK] option;
}

module type INPUT = sig
  val to_string : options -> string list -> string
  val query_input_options : unit -> unit
  val setup : string -> options -> string list -> Types.source
end
