(* virt-v2v
 * Copyright (C) 2009-2023 Red Hat Inc.
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

(** Common Linux functions. *)

val augeas_reload : Guestfs.guestfs -> unit
(** Wrapper around [g#aug_load], which (if verbose) provides
    additional debugging information about parsing problems
    that augeas found. *)

val remove : Guestfs.guestfs -> string -> string list -> unit
(** [remove g root pkgs] uninstalls the package(s). *)

val file_list_of_package : Guestfs.guestfs -> string ->
                           Guestfs.application2 -> string list
(** [file_list_of_package g root app] returns the list of files
    owned by a package. *)

val is_file_owned : Guestfs.guestfs -> string -> string -> bool
(** [is_file_owned g root path] returns true if the file is owned
    by any installed package. *)

val is_package_manager_save_file : string -> bool
(** Return true if the filename is something like [*.rpmsave], ie.
    a package manager save-file. *)
