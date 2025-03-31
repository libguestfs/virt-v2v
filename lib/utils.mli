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

(** Utilities used in virt-v2v only. *)

val large_tmpdir : string
(** [VIRT_V2V_TMPDIR] or [/var/tmp].  Create all large temporary files
    such as overlays in this directory.  Small temporary files can
    use the default behaviour eg. of {!Filename.temp_file} *)

val string_of_process_status : Unix.process_status -> string
(** Convert a process status (such as returned by {!Unix.wait}) into
    a printable string. *)

val have_selinux : bool
(** True if SELinux is enabled and enforcing on the host. *)

val uri_quote : string -> string
(** Take a string and perform %xx escaping as used in some parts of URLs. *)

val kvm_arch : string -> string
(** Map guest architecture found by inspection to the architecture
    that KVM must emulate.  Note for x86 we assume a 64 bit hypervisor. *)

val qemu_supports_sound_card : Types.source_sound_model -> bool
(** Does qemu support the given sound card? *)

val compare_app2_versions : Guestfs.application2 -> Guestfs.application2 -> int
(** Compare two app versions. *)

val du : string -> int64
(** Return the true size of a file in bytes, including any wasted
    space caused by internal fragmentation (the overhead of using
    blocks).

    This can raise either [Failure] or [Invalid_argument] in case
    of errors. *)

val qemu_img_supports_offset_and_size : unit -> bool
(** Return true iff [qemu-img] supports the ["offset"] and ["size"]
    parameters to open a subset of a file. *)

val backend_is_libvirt : unit -> bool
(** Return true iff the current backend is libvirt. *)

val chown_for_libvirt_rhbz_1045069 : string -> unit
(** If running as root, and if the backend is libvirt, libvirt
    will run qemu as a non-root user.  This prevents access
    to root-owned files and directories.  To fix this, provide
    a function to chown things we might need to qemu:root so
    qemu can access them.  Note that root normally ignores
    permissions so can still access the resource. *)

val error_if_no_ssh_agent : unit -> unit

val nbdcopy_supports_blkhash : unit -> bool
(** Return true if [nbdcopy] supports the [--blkhash] flag. *)

val create_v2v_directory : unit -> string
(** Create the directory containing inX and outX sockets. *)

val wait_for_file : string -> int -> bool
(** [wait_for_file filename timeout] waits up to [timeout] seconds for
    [filename] to appear.  It returns [true] if the file appeared. *)

val with_nbd_connect_unix : ?meta_contexts:string list ->
                            socket:string ->
                            (NBD.t -> 'a) ->
                            'a
(** [with_nbd_connect_unix socket meta_contexts f] calls function [f] with the
    NBD server at Unix domain socket [socket] connected, and the metadata
    contexts in [meta_contexts] requested (each of which is not necessarily
    supported by the server though). The connection is torn down either on
    normal return or if the function [f] throws an exception. *)

val get_disk_allocated : dir:string -> disknr:int -> int64 option
(** Callable only in the finalization step. [get_disk_allocated dir disknr]
    examines output disk [disknr] through the corresponding NBD server socket
    that resides in [dir]. Returns the number of bytes allocated in the disk
    image, according to the "base:allocation" metadata context. If the context
    is not supported by the NBD server behind the socket, the function returns
    None. *)

val get_uefi_arch_suffix : string -> string option
(** [get_uefi_arch_suffix arch] maps [arch] from [inspect.i_arch] representation
    to UEFI spec representation.  If a mapping cannot be found, [None] is
    returned. *)
