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

(** qemu-nbd as an abstract data type. *)

val is_installed : unit -> bool
(** Return true iff qemu-nbd is installed and passes some
    rudimentary tests that it is working. *)

type version = int * int * int
(** Version of qemu-nbd: [major, minor, release]. *)

val version : unit -> version
(** Get the installed version of qemu-nbd. *)

type cmd
(** A qemu-nbd command line. *)

val create : string -> cmd
(** Create a new qemu-nbd command.

    The parameter is the required filename or URI of the
    disk image to serve. *)

val set_snapshot : cmd -> bool -> unit
(** If true, set the snapshot [-s] flag.

    For safety, all input methods where [options.read_only] is true
    must set this, to ensure the input is not modified. *)

val set_format : cmd -> string option -> unit
(** Set the format [--format] parameter. *)

val set_image_opts : cmd -> bool -> unit
(** Set whether the [--image-opts] parameter is used.  This changes
    the meaning of the [filename] parameter to a set of image options.
    Consult the qemu-nbd man page for more details. *)

val run_unix : string -> cmd -> string * int
(** Start qemu-nbd command listening on a Unix domain socket,
    waiting for the process to start up.

    Returns the Unix domain socket name and the qemu-nbd process ID. *)
