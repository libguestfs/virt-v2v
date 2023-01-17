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

type i_firmware =
  | I_BIOS
  | I_UEFI of string list
(** Firmware type returned through inspection (as opposed to source
    hypervisor information which could be different or missing). *)

val detect_firmware : Guestfs.guestfs -> i_firmware
(** [detect_firmware g] sees if this guest could use UEFI to boot.  It
    should use GPT and it should have an EFI System Partition (ESP).

    If the guest has BIOS boot partition present, this is likely a BIOS+GPT
    setup, so [I_BIOS] is returned.

    If it has ESP(s), then [I_UEFI devs] is returned where [devs] is the
    list of at least one ESP.

    Otherwise, [I_BIOS] is returned. *)
