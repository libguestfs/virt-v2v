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

(** Values and functions for installing Windows virtio drivers. *)

val virtio_win_from_env : bool
(** [virtio_win_from_env] is true iff at least one of the VIRTIO_WIN and
    VIRTIO_WIN_DIR variables is present in the environment. *)

type virtio_win_installed = {
  block_driver : Types.guestcaps_block_type;
  net_driver : Types.guestcaps_net_type;
  virtio_rng : bool;
  virtio_balloon : bool;
  isa_pvpanic : bool;
  virtio_socket : bool;
}
(** After calling {!install_drivers}, this describes what virtio-win
    drivers we were able to install (and hence, what the guest requires).
    eg. if [virtio_rng] is true then we installed the virtio RNG
    device, otherwise we didn't. *)

val inject_virtio_win_drivers : Registry.t -> Types.inspect ->
                                virtio_win_installed
(** [inject_virtio_win_drivers reg inspect]
    installs virtio drivers from the driver directory or driver
    ISO into the guest driver directory and updates the registry
    so that the [viostor.sys] driver gets loaded by Windows at boot.

    [reg] is the system hive which is open for writes when this
    function is called.

    This returns a {!virtio_win_installed} struct reflecting what devices
    are now required by the guest, either virtio devices if we managed to
    install those, or legacy devices if we didn't. *)

val copy_qemu_ga : Guestfs.guestfs -> Types.inspect -> string list
(** copy MSIs (idealy just one) with QEMU Guest Agent to Windows guest. The
    MSIs are not installed by this function. *)

(**/**)

(* The following function is only exported for unit tests. *)
module UNIT_TESTS : sig
  val virtio_iso_path_matches_guest_os : string -> Types.inspect -> bool
end
