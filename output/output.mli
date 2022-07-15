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
  type poptions
  (** Opaque parsed output options. *)

  type t
  (** Opaque data that the output mode needs to pass from
      {!setup} to {!finalize}. *)

  val to_string : options -> string
  (** [to_string options] converts the destination to a printable
      string (for messages). *)

  val query_output_options : unit -> unit
  (** When the user passes [-oo ?] this is used to print help. *)

  val parse_options : options -> Types.source -> poptions
  (** [parse_options source options] should check and parse the output
      options passed on the command line.  The return value {!poptions}
      stores the parsed information and is passed to both
      {!setup} and {!finalize} methods. *)

  val setup : string -> poptions -> Types.source -> t
  (** [setup dir poptions source]

      Set up the output mode.  Sets up a disk pipeline
      [dir // "outX"] for each output disk. *)

  val finalize : string -> poptions -> t ->
                 Types.source -> Types.inspect -> Types.target_meta ->
                 unit
  (** [finalize dir poptions t inspect target_meta]

      Finalizes the conversion and writes metadata. *)

  val request_size : int option
  (** Returns an optional {i hint} about the NBD request size that
      optimizes performance.  Note that the copy program may ignore
      this.  Use NBD block size options for enforcible limits. *)
end

(** Helper functions for output modes. *)

val error_option_cannot_be_used_in_output_mode : string -> string -> unit
(** [error_option_cannot_be_used_in_output_mode mode option]
    prints error message that option cannot be used in this output mode. *)

val get_disks : string -> (int * int64) list
(** Examines the v2v directory and opens each input socket (in0 etc),
    returning a list of input disk index and size. *)

val error_if_disk_count_gt : string -> int -> unit
(** This function lets an output module enforce a maximum disk count.
    [error_if_disk_count_gt dir n] checks whether the domain has more than [n]
    disks that need to be copied, by examining the existence of input NBD socket
    "in[n]" in the v2v directory [dir].  If the socket exists, [error] is
    called. *)

type on_exit_kill = Kill | KillAndWait

val output_to_local_file : ?changeuid:((unit -> unit) -> unit) ->
                           ?compressed:bool -> ?on_exit_kill:on_exit_kill ->
                           Types.output_allocation ->
                           string -> string -> int64 -> string ->
                           unit
(** When an output mode wants to create a local file with a
    particular format (only "raw" or "qcow2" allowed) then
    this common function can be used.

    Optional parameter [?on_exit_kill] controls how the NBD server
    is cleaned up.  The default is {!Kill} which registers an
    {!On_exit.kill} handler that kills (but does not wait for)
    the server when virt-v2v exits.  Most callers should use this.

    Setting [~on_exit_kill:KillAndWait] should be used if the NBD
    server must fully exit before we continue with the rest of
    virt-v2v shut down.  This is only necessary if some other action
    (such as unmounting a host filesystem or removing a host device)
    depends on the NBD server releasing resources. *)

val disk_path : string -> string -> int -> string
(** For [-o disk|qemu], return the output disk name of the i'th disk,
    eg. 0 => /path/to/name-sda. *)
