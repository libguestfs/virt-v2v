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

type password =                 (** Use [None] for no password *)
  | AskForPassword              (** [password=-] *)
  | PasswordFile of string      (** [password=+file] *)

val create_ssh : ?bandwidth:Types.bandwidth ->
                 ?cor:string ->
                 ?retry:bool ->
                 server:string ->
                 ?port:string ->
                 ?user:string ->
                 ?password:password ->
                 string -> Nbdkit.cmd
(** Create a nbdkit object using the SSH plugin.  The required
    string parameter is the remote path.

    This can fail (calling [error]) for a variety of reasons, such
    as nbdkit not being available, wrong version, missing plugin, etc.

    Note this doesn't run nbdkit yet, it just creates the object. *)
