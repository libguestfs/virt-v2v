(* virt-v2v
 * Copyright (C) 2009-2020 Red Hat Inc.
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

(** nbdkit when used as a source. *)

val create_vddk : ?bandwidth:Types.bandwidth ->
                  ?config:string ->
                  ?cookie:string ->
                  ?cor:string ->
                  ?libdir:string ->
                  moref:string ->
                  ?nfchostport:string ->
                  ?password_file:string ->
                  ?port:string ->
                  server:string ->
                  ?snapshot:string ->
                  thumbprint:string ->
                  ?transports:string ->
                  ?user:string ->
                  string -> Nbdkit.cmd
(** Create a nbdkit object using the VDDK plugin.  The required
    string parameter is the disk remote path.

    This can fail (calling [error]) for a variety of reasons, such
    as nbdkit not being available, wrong version, missing plugin, etc.

    Note this doesn't run nbdkit yet, it just creates the object. *)
