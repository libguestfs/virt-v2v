(* helper-v2v-input
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

open Tools_utils
open Common_gettext.Gettext

type input_mode = Disk | Libvirt | LibvirtXML | OVA | VMX

let input_mode_of_string = function
  | "disk" | "local" -> Disk
  | "libvirt" -> Libvirt
  | "libvirtxml" -> LibvirtXML
  | "ova" -> OVA
  | "vmx" -> VMX
  | s -> error (f_"unknown -i option: %s") s

let select_input ?(allow_remote = true) input_mode input_conn input_transport =
  match input_mode with
  | Some Disk -> (module Input_disk.Disk : Input.INPUT)
  | Some LibvirtXML -> (module Input_libvirt.LibvirtXML)
  | Some OVA -> (module Input_ova.OVA)
  | Some VMX -> (module Input_vmx.VMX)
  | None | Some Libvirt ->
     match input_conn with
     | None -> (module Input_libvirt.Libvirt_)
     | Some orig_uri ->
        let { Xml.uri_server = server; uri_scheme = scheme } =
          try Xml.parse_uri orig_uri
          with Invalid_argument msg ->
            error (f_"could not parse '-ic %s'.  \
                      Original error message was: %s")
              orig_uri msg in

        match server, scheme, input_transport, allow_remote with
        | None, _, _, _
        | Some "", _, _, _    (* Not a remote URI. *)

        | Some _, None, _, _  (* No scheme? *)
        | Some _, Some "", _, _ ->
           (module Input_libvirt.Libvirt_)

        (* All the input method below here are remote, but virt-v2v-in-place
         * cannot work with remote disks.  If remote is not allowed
         * then fail here.
         *)
        | _, _, _, false ->
           error (f_"virt-v2v-in-place does not support remote \
                     libvirt URIs")

        (* vCenter over https. *)
        | Some server, Some ("esx"|"gsx"|"vpx"), None, true ->
           (module Input_vcenter_https.VCenterHTTPS)

        (* vCenter or ESXi using nbdkit vddk plugin *)
        | Some server, Some ("esx"|"gsx"|"vpx"), Some Input.VDDK, true ->
           (module Input_vddk.VDDK)

        (* Xen over SSH *)
        | Some server, Some "xen+ssh", _, true ->
           (module Input_xen_ssh.XenSSH)

        (* Old virt-v2v also supported qemu+ssh://.  However I am
         * deliberately not supporting this in new virt-v2v.  Don't
         * use virt-v2v if a guest already runs on KVM.
         *)

        (* Unknown remote scheme. *)
        | Some _, Some _, _, true ->
           warning (f_"no support for remote libvirt connections \
                       to '-ic %s'.  The conversion may fail when it \
                       tries to read the source disks.") orig_uri;
           (module Input_libvirt.Libvirt_)
