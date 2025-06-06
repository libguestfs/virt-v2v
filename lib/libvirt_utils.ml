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

open Std_utils
open Tools_utils
open Common_gettext.Gettext

(* This module provides helper methods on top of the Libvirt
    module. *)

let auth_for_password_file ?password_file () =
  let password = Option.map read_first_line_from_file password_file in
  let auth_fn creds =
    List.map (
      function
      | { Libvirt.Connect.typ = Libvirt.Connect.CredentialPassphrase } ->
         password
      | _ -> None
    ) creds
  in

  let base_auth = Libvirt.Connect.get_auth_default () in

  if password_file = None then
    base_auth
  else
    { base_auth with
      cb = auth_fn;
    }

let get_domain conn name =
  let dom =
    try
      Libvirt.Domain.lookup_by_uuid_string conn name
    with
    (* No such domain. *)
    | Libvirt.Virterror { code = VIR_ERR_NO_DOMAIN }
    (* Invalid UUID string. *)
    | Libvirt.Virterror { code = VIR_ERR_INVALID_ARG; domain = VIR_FROM_DOMAIN } ->
      (try
        Libvirt.Domain.lookup_by_name conn name
      with
        Libvirt.Virterror { code = VIR_ERR_NO_DOMAIN; message } ->
          error (f_"cannot find libvirt domain ‘%s’: %s")
            name (Option.value ~default:"" message)
      ) in

  (* As a side-effect we check that the domain is shut down (RHBZ#1138586).
   * In earlier versions of virt-v2v this was a hard error.  Now it's
   * a warning, since we can't easily tell if the user is converting
   * from a snapshot - which is safe (RHEL-88543).
   *)
  let uri = Libvirt.Connect.get_uri conn in
  if not (String.starts_with "test:" uri) then (
    (match (Libvirt.Domain.get_info dom).Libvirt.Domain.state with
     | InfoRunning | InfoBlocked | InfoPaused ->
        warning (f_"libvirt domain ‘%s’ is running or paused.  Converting \
                    a live guest will result in corrupted output. \
                    However this is safe if you're converting from a \
                    snapshot")
          (Libvirt.Domain.get_name dom)
     | InfoNoState | InfoShutdown | InfoShutoff | InfoCrashed
     | InfoPMSuspended ->
        ()
    )
  );

  dom

let get_pool conn name =
  try
    Libvirt.Pool.lookup_by_uuid_string conn name
  with
  (* No such pool. *)
  | Libvirt.Virterror { code = VIR_ERR_NO_STORAGE_POOL }
  (* Invalid UUID string. *)
  | Libvirt.Virterror { code = VIR_ERR_INVALID_ARG; domain = VIR_FROM_STORAGE } ->
    (try
      Libvirt.Pool.lookup_by_name conn name
    with Libvirt.Virterror { code = VIR_ERR_NO_STORAGE_POOL; message } ->
      error (f_"cannot find libvirt pool ‘%s’: %s\n\nUse \
                ‘virsh pool-list --all’ to list all available pools, \
                and ‘virsh pool-dumpxml <pool>’ to display details \
                about a particular pool.\n\nTo set the pool which \
                virt-v2v uses, add the ‘-os <pool>’ option.")
        name (Option.value ~default:"" message)
    )

let get_volume pool name =
  try
    Libvirt.Volume.lookup_by_name pool name
  with
  (* No such volume. *)
  | Libvirt.Virterror { code = VIR_ERR_NO_STORAGE_VOL; message } ->
    error (f_"cannot find libvirt volume ‘%s’: %s")
      name (Option.value ~default:"" message)

let domain_exists conn dom =
  try
    ignore (Libvirt.Domain.lookup_by_name conn dom);
    true
  with
    Libvirt.Virterror { code = VIR_ERR_NO_DOMAIN } -> false

let libvirt_get_version () =
  let v, _ = Libvirt.get_version () in
  let v_major = v / 1000000 in
  let v_minor = (v / 1000) mod 1000 in
  let v_micro = v mod 1000 in
  (v_major, v_minor, v_micro)
