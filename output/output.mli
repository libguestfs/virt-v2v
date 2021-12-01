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

  val cleanup : unit -> unit
  (** The main program will call this function on exit. *)
end

module Disk : OUTPUT             (** [-o disk] output mode. *)
module Glance : OUTPUT           (** [-o glance] output mode. *)
module Json : OUTPUT             (** [-o json] output mode. *)
module Libvirt_ : OUTPUT         (** [-o libvirt] output mode. *)
module Null : OUTPUT             (** [-o null] output mode. *)
module Openstack : OUTPUT        (** [-o openstack] output mode. *)
module QEMU : OUTPUT             (** [-o qemu] output mode. *)
module RHVUpload : OUTPUT        (** [-o rhv-upload] output mode. *)
module RHV : OUTPUT              (** [-o rhv] output mode. *)
module VDSM : OUTPUT             (** [-o vdsm] output mode. *)
