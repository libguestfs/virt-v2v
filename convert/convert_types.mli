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

module type CONVERT = sig
  val name : string
  (** Module name (only used in debugging). *)

  val convert : Guestfs.guestfs -> Types.source -> Types.inspect ->
                Firmware.i_firmware -> Types.guestcaps_block_type ->
                bool -> Types.static_ip list ->
                Types.guestcaps
  (** Perform the guest-specific conversion for Linux or Windows.
      This function is called with the guest disks mounted. *)

  val post_convert : Guestfs.guestfs -> Types.inspect -> unit
  (** Perform "post-conversion" operations.  This is only used for
      Windows, where some operations must be done after the disks
      have been unmounted.  For Linux it does nothing. *)
end
(** Conversion modules for Linux and Windows are provided separately.
    They both provide this interface. *)
