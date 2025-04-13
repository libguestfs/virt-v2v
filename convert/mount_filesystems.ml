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

let rec mount_filesystems g root =
  reject_if_not_installed_image g root;
  reject_if_unknown_fields g root;

  (* Mount up the filesystems. *)
  let mps = g#inspect_get_mountpoints root in
  let cmp (a,_) (b,_) = compare (String.length a) (String.length b) in
  let mps = List.sort cmp mps in
  List.iter (
    fun (mp, dev) ->
      (try g#mount dev mp
       with G.Error msg ->
         if mp = "/" then ( (* RHBZ#1145995 *)
           if String.find msg "Windows" >= 0 &&
                String.find msg "NTFS partition is in an unsafe state" >= 0 then
             error (f_"unable to mount the disk image for writing. This has \
                       probably happened because Windows Hibernation or \
                       Fast Restart is being used in this guest. You have \
                       to disable this (in the guest) in order to use \
                       virt-v2v.\n\nOriginal error message: %s") msg
           else
             error "%s" msg
         )
         else
           warning (f_"%s (ignored)") msg
      );

      (* Some filesystems (hello, ntfs-3g) can silently fall back to
       * a read-only mount.  Check the root filesystem is really writable.
       * RHBZ#1567763
       *)
      if mp = "/" then (
        let file = sprintf "/%s" (String.random8 ()) in
        (try g#touch file
         with G.Error msg ->
           if g#last_errno () = G.Errno.errno_EROFS then
             error (f_"filesystem was mounted read-only, even though we \
                       asked for it to be mounted read-write.  This usually \
                       means that the filesystem was not cleanly unmounted.  \
                       Possible causes include trying to convert a guest \
                       which is running, or using Windows Hibernation or \
                       Fast Restart.\n\nOriginal error message: %s") msg
           else
             error (f_"could not write to the guest filesystem: %s") msg
        );
        g#rm file
      )
  ) mps;

  (* Get list of applications/packages installed. *)
  let package_format = g#inspect_get_package_format root in
  let apps = list_applications g root package_format in
  let apps = Array.to_list apps in

  (* A map of app2_name -> application2, for easier lookups.  Note
   * that app names are not unique!  (eg. 'kernel' can appear multiple
   * times)
   *)
  let apps_map = List.fold_left (
    fun map app ->
      let name = app.G.app2_name in
      let vs = try StringMap.find name map with Not_found -> [] in
      StringMap.add name (app :: vs) map
  ) StringMap.empty apps in

  let drive_mappings = g#inspect_get_drive_mappings root in

  (* If the guest is Windows, get some Windows-specific inspection
   * data, else (for simplicity when accessing) use empty strings.
   *)
  let typ = g#inspect_get_type root in
  let systemroot, software_hive, system_hive, current_cs =
    match typ with
    | "windows" ->
       g#inspect_get_windows_systemroot root,
       g#inspect_get_windows_software_hive root,
       g#inspect_get_windows_system_hive root,
       g#inspect_get_windows_current_control_set root
    | _ ->
       "", "", "", "" in

  let inspect = {
    i_root = root;
    i_type = typ;
    i_distro = g#inspect_get_distro root;
    i_osinfo = g#inspect_get_osinfo root;
    i_arch = g#inspect_get_arch root;
    i_major_version = g#inspect_get_major_version root;
    i_minor_version = g#inspect_get_minor_version root;
    i_package_format = package_format;
    i_package_management = g#inspect_get_package_management root;
    i_product_name = g#inspect_get_product_name root;
    i_product_variant = g#inspect_get_product_variant root;
    i_mountpoints = mps;
    i_apps = apps;
    i_apps_map = apps_map;
    i_windows_systemroot = systemroot;
    i_windows_software_hive = software_hive;
    i_windows_system_hive = system_hive;
    i_windows_current_control_set = current_cs;
    i_drive_mappings = drive_mappings;
  } in
  debug "%s" (string_of_inspect inspect);

  inspect

(* Reject this OS if it doesn't look like an installed image. *)
and reject_if_not_installed_image g root =
  let fmt = g#inspect_get_format root in
  if fmt <> "installed" then
    error (f_"libguestfs thinks this is not an installed operating \
              system (it might be, for example, an installer disk \
              or live CD).  If this is wrong, it is probably a bug \
              in libguestfs.  root=%s fmt=%s") root fmt

(* If some inspection fields are "unknown", then that indicates a
 * failure in inspection, and we shouldn't continue.  For an example
 * of this, see RHBZ#1278371.  However don't "assert" here, since
 * the user might have pointed virt-v2v at a blank disk.  Give an
 * error message instead.
 *)
and reject_if_unknown_fields g root =
  error_if_unknown "i_type" (g#inspect_get_type root);
  error_if_unknown "i_distro" (g#inspect_get_distro root);
  error_if_unknown "i_arch" (g#inspect_get_arch root)

and error_if_unknown fieldname value =
  if value = "unknown" then
    error (f_"inspection could not detect the source guest (or \
              physical machine) operating system.\n\n\
              Assuming that you are running virt-v2v/virt-p2v \
              on a source which is supported (and not, for example, \
              a blank disk), then this should not happen.\n\n\
              Inspection field ‘%s’ was ‘unknown’.")
          fieldname

(* Wrapper around g#inspect_list_applications2 which, for RPM
 * guests, on failure tries to rebuild the RPM database before
 * repeating the operation.
 *)
and list_applications g root = function
  | "rpm" ->
     (* RPM guest.
      *
      * In libguestfs before commit 488245ed6c ("daemon: rpm: Check
      * return values from librpm calls"), a corrupt RPM database
      * would return an empty array here with no exception.  Hence
      * the check below which turns empty array => exception.  In
      * libguestfs after that commit, inspect_list_applications2
      * will raise an exception if it detects a corrupt RPM database.
      *)
     (try
        let apps = g#inspect_list_applications2 root in
        if apps = [||] then raise (G.Error "no applications returned");
        apps
      with G.Error msg ->
        debug "%s" msg;
        debug "rebuilding RPM database and retrying ...";
        ignore (g#sh "rpmdb --rebuilddb");
        g#inspect_list_applications2 root
     )
  | _ ->
     (* Non-RPM guest, just do it. *)
     g#inspect_list_applications2 root
