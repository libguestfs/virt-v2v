(* virt-v2v
 * Copyright (C) 2009-2024 Red Hat Inc.
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

(** Wrappers for finding and downloading remote files over ssh. *)

(** [remote_file_exists password ?port server ?user path]
    checks that [path] exists on the remote server. *)
val remote_file_exists : password:Nbdkit_ssh.password ->
                         ?port:string -> server:string -> ?user:string ->
                         string -> bool

(** [download_file password ?port server ?user path output]
    uses scp to copy the single remote file at [path] to
    the local file called [output]. *)
val download_file : password:Nbdkit_ssh.password ->
                    ?port:string -> server:string -> ?user:string -> string ->
                    string -> unit