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

open Std_utils

open Printf

type t = Unix of string * string option

let to_uri (Unix (socket, export)) =
  let export =
    export |> Option.map Utils.uri_quote |> Option.value ~default:"" in
  (* We assume here that the socket filename does not contain any
   * characters that need escaping, which is a safe assumption
   * for v2vdir and sockets in that directory.
   *)
  sprintf "nbd+unix:///%s?socket=%s" export socket
