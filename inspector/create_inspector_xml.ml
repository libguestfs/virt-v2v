(* virt-v2v-inspector
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

open Types
open Utils
open DOM

(* This is where we construct the final XML document based on
 * these inputs:
 *   - Global configuration like the version of v2v etc.
 *   - The NBD input disks
 *   - The inspection data (Types.inspect)
 *)
let rec create_inspector_xml input_disks inspect target_meta =
  let body = ref [] in

  (* Record the version of virt-v2v etc, mainly for debugging. *)
  List.push_back_list body [
    Comment generated_by;
    e "program" [] [PCData "virt-v2v-inspector"];
    e "package" [] [PCData Config.package_name];
    e "version" [] [PCData Config.package_version];
  ];

  (* The disks. *)
  let disks = ref [] in

  List.iteri (
    fun index uri ->
      let uri = NBD_URI.to_uri uri in
      let virtual_size = Utils.with_nbd_connect_uri ~uri NBD.get_size in

      let elems = ref [] in
      List.push_back elems (e "virtual-size" []
                              [PCData (Int64.to_string virtual_size)]);
      (match get_disk_allocated uri with
       | None -> ()
       | Some real_size ->
          List.push_back elems (e "allocated" [ "estimated", "true" ]
                                  [PCData (Int64.to_string real_size)])
      );

      List.push_back disks (e "disk" [ "index", string_of_int index ] !elems)
  ) input_disks;
  List.push_back body (e "disks" [] !disks);

  (* The <firmware> field is outside the <operatingsystem> element,
   * since firmware is not part of the OS, and also because this is
   * consistent with virt-drivers output.
   *)
  List.push_back body
    (e "firmware"
       ["type",
        string_of_target_firmware target_meta.target_firmware]
       []);

  (* The inspection data. *)
  (* NB: Keep these field names compatible with virt-inspector! *)
  let os = ref [] in
  List.push_back os (e "name" [] [PCData inspect.i_type]);
  List.push_back os (e "distro" [] [PCData inspect.i_distro]);
  List.push_back os (e "osinfo" [] [PCData inspect.i_osinfo]);
  List.push_back os (e "arch" [] [PCData inspect.i_arch]);
  List.push_back os (e "major_version" []
                       [PCData (string_of_int inspect.i_major_version)]);
  List.push_back os (e "minor_version" []
                       [PCData (string_of_int inspect.i_minor_version)]);
  if inspect.i_package_format <> "" then
    List.push_back os (e "package_format" []
                         [PCData inspect.i_package_format]);
  if inspect.i_package_management <> "" then
    List.push_back os (e "package_management" []
                         [PCData inspect.i_package_management]);
  if inspect.i_product_name <> "" then
    List.push_back os (e "product_name" [] [PCData inspect.i_product_name]);
  if inspect.i_product_variant <> "" then
    List.push_back os (e "product_variant" []
                         [PCData inspect.i_product_variant]);

  if inspect.i_windows_systemroot <> "" then
    List.push_back os (e "windows_systemroot" []
                         [PCData inspect.i_windows_systemroot]);
  if inspect.i_windows_software_hive <> "" then
    List.push_back os (e "windows_software_hive" []
                         [PCData inspect.i_windows_software_hive]);
  if inspect.i_windows_systemroot <> "" then
    List.push_back os (e "windows_system_hive" []
                         [PCData inspect.i_windows_system_hive]);
  if inspect.i_windows_current_control_set <> "" then
    List.push_back os (e "windows_current_control_set" []
                         [PCData inspect.i_windows_current_control_set]);

  List.push_back os (e "root" [] [PCData inspect.i_root]);
  let mps = ref [] in
  List.iter (
    fun (fs, dev) ->
      List.push_back mps (e "mountpoint" [ "dev", dev ] [PCData fs])
  ) inspect.i_mountpoints;
  List.push_back os (e "mountpoints" [] !mps);

  List.push_back body (e "operatingsystem" [] !os);

  (* Construct the final document. *)
  (doc "v2v-inspection" [] !body : DOM.doc)
