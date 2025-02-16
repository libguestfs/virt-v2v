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

type options = {
  bandwidth : Types.bandwidth option;     (* [--bandwidth] option *)
  input_conn : string option;             (* [-ic] option *)
  input_format : string option;           (* [-if] option *)
  input_options : (string * string) list; (* [-io] options *)
  input_password : string option;         (* [-ip] option *)
  input_transport : [`SSH|`VDDK] option;  (* [-it] option *)
  read_only : bool;                       (* read-only (usually true) *)
}

module type INPUT = sig
  val to_string : options -> string list -> string
  (** [to_string options args] converts the source to a printable
      string (for messages). *)

  val query_input_options : unit -> unit
  (** When the user passes [-io ?] this is used to print help. *)

  val setup : string -> options -> string list -> Types.source
  (** [setup dir options args]

      Set up the input mode.  Examines the source and extracts
      source metadata ([Types.source]).  Creates a disk pipeline
      [dir // "inX"] for each input disk. *)
end
