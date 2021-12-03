(* virt-v2v
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
  output_alloc : Types.output_allocation;
  output_conn : string option;
  output_format : string;
  output_options : (string * string) list;
  output_name : string option;
  output_password : string option;
  output_storage : string option;
}

module type OUTPUT = sig
  type t
  (** Opaque data used by the output mode. *)

  val setup : string -> options -> Types.source -> t
  (** [setup dir options source]

      Set up the output mode.  Sets up a disk pipeline
      [dir // "outX"] for each output disk. *)

  val finalize : string -> options ->
                 Types.source -> Types.inspect -> Types.target_meta ->
                 t ->
                 unit
  (** [finalize dir inspect target_meta t]

      Finalizes the conversion and writes metadata. *)

  val query_output_options : unit -> unit
  (** When the user passes [-oo ?] this is used to print help. *)
end

(** Helper functions for output modes. *)

val error_option_cannot_be_used_in_output_mode : string -> string -> unit
(** [error_option_cannot_be_used_in_output_mode mode option]
    prints error message that option cannot be used in this output mode. *)

val get_output_name : options -> Types.source -> string
(** Works out the output name from -on or input name. *)

val get_disks : string -> (int * int64) list
(** Examines the v2v directory and opens each input socket (in0 etc),
    returning a list of input disk index and size. *)

val output_to_local_file : ?changeuid:((unit -> unit) -> unit) ->
                           Types.output_allocation ->
                           string -> string -> int64 -> string ->
                           unit
(** When an output mode wants to create a local file with a
    particular format (only "raw" or "qcow2" allowed) then
    this common function can be used. *)

val disk_path : string -> string -> int -> string
(** For [-o disk|qemu], return the output disk name of the i'th disk,
    eg. 0 => /path/to/name-sda. *)
