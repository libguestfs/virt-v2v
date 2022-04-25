(* virt-v2v
 * Copyright (C) 2009-2022 Red Hat Inc.
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

(** Simple YAML generator.

    This doesn't attempt to model all of YAML, only enough
    to be able to generate Kubevirt machine descriptions.
*)

type node =
  | Assoc of (string * node) list  (* dictionary of key: value *)
  | List of node list              (* list of - nodes *)
  | String of string               (* simple string *)
  | Int of int                     (* integer *)
  | Bool of bool                   (* bool - NB. "Norway problem" *)
  | Float of float                 (* float *)
  | Block of string list           (* block of lines *)
type doc = Doc of node

val doc_to_string : doc -> string
(** Convert a document to a string representation. *)

val doc_to_chan : out_channel -> doc -> unit
(** Write the YAML document to an output channel. *)
