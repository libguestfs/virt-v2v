(* virt-v2v
 * Copyright (C) 2011-2025 Red Hat Inc.
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

(* This file tests individual virt-v2v functions. *)

open Printf

open Std_utils
open Tools_utils

open Types
open Utils

let assert_equal ?(cmp = fun a b -> a = b) ~printer a b =
  if not (cmp a b) then
    failwithf "FAIL: %s <> %s" (printer a) (printer b)

let assert_bool name b =
  if not b then failwithf "FAIL: %s" name

let inspect_defaults = {
  i_type = ""; i_distro = ""; i_osinfo = ""; i_arch = "";
  i_major_version = 0; i_minor_version = 0;
  i_root = ""; i_package_format = ""; i_package_management = "";
  i_product_name = ""; i_product_variant = ""; i_mountpoints = [];
  i_filesystems = [];
  i_apps = []; i_apps_map = StringMap.empty;
  i_windows_systemroot = "";
  i_windows_software_hive = ""; i_windows_system_hive = "";
  i_windows_current_control_set = ""; i_windows_group_policy = false;
  i_drive_mappings = [];
}

(* Test Create_ovf.get_ostype *)
let () =
  let printer = Fun.id in
  assert_equal ~printer "RHEL6"
               (Create_ovf.get_ostype {
                    inspect_defaults with
                    i_type = "linux"; i_distro = "rhel";
                    i_major_version = 6;
                    i_minor_version = 0;
                    i_arch = "i386" });
  assert_equal ~printer "RHEL6x64"
               (Create_ovf.get_ostype {
                    inspect_defaults with
                    i_type = "linux"; i_distro = "rhel";
                    i_major_version = 6;
                    i_minor_version = 0;
                    i_arch = "x86_64" });
  assert_equal ~printer "rhel_7x64"
               (Create_ovf.get_ostype {
                    inspect_defaults with
                    i_type = "linux"; i_distro = "rhel";
                    i_major_version = 7;
                    i_minor_version = 0;
                    i_arch = "x86_64" });
  assert_equal ~printer "Windows7"
               (Create_ovf.get_ostype {
                    inspect_defaults with
                    i_type = "windows";
                    i_major_version = 6;
                    i_minor_version = 1;
                    i_product_variant = "Client";
                    i_arch = "i386" });
  assert_equal ~printer "Windows7x64"
               (Create_ovf.get_ostype {
                    inspect_defaults with
                    i_type = "windows";
                    i_major_version = 6;
                    i_minor_version = 1;
                    i_product_variant = "Client";
                    i_arch = "x86_64" });
  assert_equal ~printer "windows_8"
               (Create_ovf.get_ostype {
                    inspect_defaults with
                    i_type = "windows";
                    i_major_version = 6;
                    i_minor_version = 2;
                    i_product_variant = "Client";
                    i_arch = "i386" });
  assert_equal ~printer "windows_8x64"
               (Create_ovf.get_ostype {
                    inspect_defaults with
                    i_type = "windows";
                    i_major_version = 6;
                    i_minor_version = 2;
                    i_product_variant = "Client";
                    i_arch = "x86_64" });
  assert_equal ~printer "windows_2012x64"
               (Create_ovf.get_ostype {
                    inspect_defaults with
                    i_type = "windows";
                    i_major_version = 6;
                    i_minor_version = 2;
                    i_product_variant = "Server";
                    i_arch = "x86_64" });
  assert_equal ~printer "windows_2012R2x64"
               (Create_ovf.get_ostype {
                    inspect_defaults with
                    i_type = "windows";
                    i_major_version = 6;
                    i_minor_version = 3;
                    i_product_variant = "Server";
                    i_arch = "x86_64" })

(* Test Utils.qemu_img_supports_offset_and_size *)
let () =
  (* No assertion here, we don't know if qemu-img supports the
   * feature, so just run the code and make sure it doesn't crash.
   *)
  ignore (Utils.qemu_img_supports_offset_and_size ())

(* Test the VMX file parser in the VMX module. *)
let () =
  let cmp = VMX.equal in
  let printer = VMX.to_string 0 in

  (* This should be identical to the empty file. *)
  let t = VMX.parse_string "\
test.foo = \"a\"
test.bar = \"b\"
test.present = \"FALSE\"
" in
  assert_equal ~cmp ~printer VMX.empty t;

  (* Test weird escapes. *)
  let t1 = VMX.parse_string "\
foo = \"a|20|21b\"
" in
  let t2 = VMX.parse_string "\
foo = \"a !b\"
" in
  assert_equal ~cmp ~printer t1 t2;

  (* Test case insensitivity. *)
  let t1 = VMX.parse_string "\
foo = \"abc\"
" in
  let t2 = VMX.parse_string "\
fOO = \"abc\"
" in
  assert_equal ~cmp ~printer t1 t2;
  let t = VMX.parse_string "\
flag = \"true\"
" in
  assert_bool "VMX: failed case insensitivity test for booleans #1"
              (VMX.get_bool t ["FLAG"] = Some true);
  let t = VMX.parse_string "\
flag = \"TRUE\"
" in
  assert_bool "VMX: failed case insensitivity test for booleans #2"
              (VMX.get_bool t ["Flag"] = Some true);

  (* Missing keys. *)
  let t = VMX.parse_string "\
foo = \"a\"
" in
  assert_bool "VMX: failed missing key test"
              (VMX.get_string t ["bar"] = None);

  (* namespace_present function *)
  let t = VMX.parse_string "\
foo.bar.present = \"TRUE\"
foo.baz.present = \"FALSE\"
foo.a.b = \"abc\"
foo.a.c = \"abc\"
foo.b = \"abc\"
foo.c.a = \"abc\"
foo.c.b = \"abc\"
" in
 assert_bool "VMX: namespace_present #1"
             (VMX.namespace_present t ["foo"] = true);
 assert_bool "VMX: namespace_present #2"
             (VMX.namespace_present t ["foo"; "bar"] = true);
 assert_bool "VMX: namespace_present #3"
             (* this whole namespace should have been culled *)
             (VMX.namespace_present t ["foo"; "baz"] = false);
 assert_bool "VMX: namespace_present #4"
             (VMX.namespace_present t ["foo"; "a"] = true);
 assert_bool "VMX: namespace_present #5"
             (* this is a key, not a namespace *)
             (VMX.namespace_present t ["foo"; "a"; "b"] = false);
 assert_bool "VMX: namespace_present #6"
             (VMX.namespace_present t ["foo"; "b"] = false);
 assert_bool "VMX: namespace_present #7"
             (VMX.namespace_present t ["foo"; "c"] = true);
 assert_bool "VMX: namespace_present #8"
             (VMX.namespace_present t ["foo"; "d"] = false);

 (* map function *)
  let t = VMX.parse_string "\
foo.bar.present = \"TRUE\"
foo.baz.present = \"FALSE\"
foo.a.b = \"abc\"
foo.a.c = \"abc\"
foo.b = \"abc\"
foo.c.a = \"abc\"
foo.c.b = \"abc\"
" in
  let xs =
    VMX.map (
      fun path ->
        let path = String.concat "." path in
        function
        | None -> sprintf "%s.present = \"true\"\n" path
        | Some v -> sprintf "%s = \"%s\"\n" path v
    ) t in
  let xs = List.sort compare xs in
  let s = String.concat "" xs in
  assert_equal ~printer:Fun.id "\
foo.a.b = \"abc\"
foo.a.c = \"abc\"
foo.a.present = \"true\"
foo.b = \"abc\"
foo.bar.present = \"TRUE\"
foo.bar.present = \"true\"
foo.c.a = \"abc\"
foo.c.b = \"abc\"
foo.c.present = \"true\"
foo.present = \"true\"
" s;

  (* select_namespaces function *)
  let t1 = VMX.parse_string "\
foo.bar.present = \"TRUE\"
foo.a.b = \"abc\"
foo.a.c = \"abc\"
foo.b = \"abc\"
foo.c.a = \"abc\"
foo.c.b = \"abc\"
" in
  let t2 =
    VMX.select_namespaces
      (function ["foo"] -> true | _ -> false) t1 in
  assert_equal ~cmp ~printer t1 t2;

  let t1 = VMX.parse_string "\
foo.bar.present = \"TRUE\"
foo.a.b = \"abc\"
foo.a.c = \"abc\"
foo.b = \"abc\"
foo.c.a = \"abc\"
foo.c.b = \"abc\"
foo.c.c.d.e.f = \"abc\"
" in
  let t1 =
    VMX.select_namespaces
      (function ["foo"; "a"] -> true | _ -> false) t1 in
  let t2 = VMX.parse_string "\
foo.a.b = \"abc\"
foo.a.c = \"abc\"
" in
  assert_equal ~cmp ~printer t2 t1
