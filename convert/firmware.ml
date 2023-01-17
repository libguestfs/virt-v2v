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

open Printf

open Tools_utils

module G = Guestfs

type i_firmware =
  | I_BIOS
  | I_UEFI of string list

let detect_firmware g =
  let parttype_is_gpt dev =
    try g#part_get_parttype dev = "gpt"
    with G.Error msg as exn ->
         (* If it's _not_ "unrecognised disk label" then re-raise it. *)
         if g#last_errno () <> G.Errno.errno_EINVAL then raise exn;
         debug "%s (ignored)" msg;
         false
  in
  let accumulate_partition (esp_parts, bboot) part =
    let dev = g#part_to_dev part in
    if parttype_is_gpt dev then
      let partnum = g#part_to_partnum part in
      let part_type_guid = g#part_get_gpt_type dev partnum in
      match part_type_guid with
      (* EFI system partition *)
      | "C12A7328-F81F-11D2-BA4B-00A0C93EC93B" -> part :: esp_parts, bboot
      (* BIOS boot partition *)
      | "21686148-6449-6E6F-744E-656564454649" -> esp_parts, true
      | _ -> esp_parts, bboot
    else esp_parts, bboot
  in

  let esp_partitions, bios_boot =
    Array.fold_left accumulate_partition ([], false) (g#list_partitions ()) in

  (* If there's a BIOS boot partition present (0xef02 type for gdisk,
   * "bios_grub" flag for parted), then this is likely a BIOS+GPT setup.
   * In this case we prioritize BIOS boot partition and detect BIOS firmware,
   * no matter how many ESPs we've found.
   *)
  match esp_partitions, bios_boot with
  | _ :: _, false -> I_UEFI esp_partitions
  | _ -> I_BIOS
