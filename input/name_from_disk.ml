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

open Common_gettext.Gettext
open Tools_utils

let name_from_disk disk =
  let name = Filename.basename disk in
  (* Remove the extension (or suffix), only if it's one usually
   * used for disk images. *)
  let suffixes = [
    ".img"; ".ova"; ".qcow2"; ".raw"; ".vmdk"; ".vmx";
    "-sda";
  ] in
  let rec loop = function
    | suff :: xs ->
       if Filename.check_suffix name suff then
         Filename.chop_suffix name suff
       else
         loop xs
    | [] -> name
  in
  let name = loop suffixes in
  if name = "" then
    error (f_"invalid input filename (%s)") disk;
  name
