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

open Printf

open Std_utils
open Tools_utils
open Common_gettext.Gettext

module G = Guestfs

open Types

let choose_root root_choice g =
  let roots = g#inspect_os () in
  let roots = Array.to_list roots in

  match roots with
  | [] ->
     error (f_"inspection could not detect the source guest \
               (or physical machine) operating system.\n\n\
               Assuming that you are running virt-v2v/virt-p2v \
               on a source which is supported (and not, for example, \
               a blank disk), then this should not happen.\n\n\
               No root device found in this operating system image.");
  | [root] -> root (* only one root, so return it *)
  | roots ->
     (* If there are multiple roots, use the [--root] option supplied
      * by the user to help us choose what we should do next.
      *)
     match root_choice with
     | AskRoot ->
        (* List out the roots and ask the user to choose. *)
        printf "\n***\n";
        printf (f_"Dual- or multi-boot operating system detected.  \
                   Choose the root filesystem\nthat contains the main \
                   operating system from the list below:\n");
        printf "\n";
        List.iteri (
          fun i root ->
            let prod = g#inspect_get_product_name root in
            match prod with
            | "unknown" -> printf " [%d] %s\n" (i+1) root
            | prod -> printf " [%d] %s (%s)\n" (i+1) root prod
        ) roots;
        printf "\n";
        let i = ref 0 in
        let n = List.length roots in
        while !i < 1 || !i > n do
          printf (f_"Enter a number between 1 and %d, or ‘exit’: ") n;
          let input = read_line () in
          if input = "exit" || input = "q" || input = "quit" then
            exit 1
          else (
            try i := int_of_string input
            with
            | End_of_file -> error (f_"connection closed")
            | Failure _ -> ()
          )
        done;
        List.nth roots (!i - 1)

      | SingleRoot ->
        error (f_"multi-boot operating systems are not supported by \
                  virt-v2v. Use the --root option to change how virt-v2v \
                  handles this.")

      | FirstRoot ->
        let root = List.hd roots in
        info (f_"Picked %s because '--root first' was used.") root;
        root

      | RootDev dev ->
        let root =
          if List.mem dev roots then dev
          else
            error (f_"root device %s not found.  Roots found were: %s")
              dev (String.concat " " roots) in
        info (f_"Picked %s because '--root %s' was used.") root dev;
        root
