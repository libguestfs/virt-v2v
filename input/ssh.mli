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

(** Wrappers for finding and downloading remote files over ssh.

    Internally this uses nbdkit-ssh-plugin (which uses sftp) as
    that is much more predictable than running external ssh / scp. *)

(** [remote_file_exists server ?port ?user ?password path]
    checks that [path] exists on the remote server. *)
val remote_file_exists : server:string -> ?port:string ->
                         ?user:string -> ?password:Nbdkit_ssh.password ->
                         string -> bool

(** [download_file server ?port ?user ?password path output]
    downloads the single remote file at [path] to
    the local file called [output]. *)
val download_file : server:string -> ?port:string ->
                    ?user:string -> ?password:Nbdkit_ssh.password ->
                    string ->
                    string -> unit
