(* virt-v2v
 * Copyright (C) 2017-2025 Red Hat Inc.
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
open Scanf

open Std_utils
open Tools_utils
open Unix_utils
open Common_gettext.Gettext

open Types
open Utils
open Name_from_disk

type vmx_source =
  | VMXSourceFile of string     (* local file or NFS *)
  | VMXSourceSSH of Nbdkit_ssh.password option * Xml.uri (* SSH URI *)

(* The single filename on the command line is interpreted either as
 * a local file or a remote SSH URI (only if ‘-it ssh’).
 *)
let vmx_source_of_arg input_password input_transport arg =
  match input_transport, arg with
  | None, arg -> VMXSourceFile arg
  | Some `SSH, arg ->
     let uri =
       try Xml.parse_uri arg
       with Invalid_argument _ ->
         error (f_"remote vmx ‘%s’ could not be parsed as a URI") arg in
     if uri.Xml.uri_scheme <> None && uri.Xml.uri_scheme <> Some "ssh" then
       error (f_"vmx URI start with ‘ssh://...’");
     if uri.Xml.uri_server = None then
       error (f_"vmx URI remote server name omitted");
     if uri.Xml.uri_path = None || uri.Xml.uri_path = Some "/" then
       error (f_"vmx URI path component looks incorrect");
     VMXSourceSSH (input_password, uri)

let rec find_disks vmx vmx_source =
  (* Set the s_disk_id field to an incrementing number. *)
  List.mapi
    (fun i (source, filename) -> { source with s_disk_id = i }, filename)
    (find_scsi_disks vmx vmx_source @
     find_nvme_disks vmx vmx_source @
     find_sata_disks vmx vmx_source @
     find_ide_disks vmx vmx_source)

(* Find all SCSI hard disks.
 *
 * In the VMX file:
 *   scsi0.virtualDev = "pvscsi"  # or may be "lsilogic" etc.
 *   scsi0:0.deviceType = "disk" | "plainDisk" | "rawDisk" | "scsi-hardDisk"
 *                        | omitted
 *   scsi0:0.fileName = "guest.vmdk"
 *)
and find_scsi_disks vmx vmx_source =
  let get_scsi_controller_target ns =
    sscanf ns "scsi%d:%d" (fun c t -> c, t)
  in
  let is_scsi_controller_target ns =
    try ignore (get_scsi_controller_target ns); true
    with Scanf.Scan_failure _ | End_of_file | Failure _ -> false
  in
  let scsi_device_types = [ Some "disk"; Some "plaindisk"; Some "rawdisk";
                            Some "scsi-harddisk"; None ] in
  let scsi_controller = Source_SCSI in

  find_hdds vmx vmx_source
            get_scsi_controller_target is_scsi_controller_target
            scsi_device_types scsi_controller

(* Find all NVMe hard disks.
 *
 * In the VMX file:
 *   nvme0.pcislotnumber = "192"
 *   nvme0:0.fileName = "guest.vmdk"
 *)
and find_nvme_disks vmx vmx_source =
  let get_nvme_controller_target ns =
    sscanf ns "nvme%d:%d" (fun c t -> c, t)
  in
  let is_nvme_controller_target ns =
    try ignore (get_nvme_controller_target ns); true
    with Scanf.Scan_failure _ | End_of_file | Failure _ -> false
  in
  let nvme_device_types = [ None ] in
  let nvme_controller = Source_NVME in

  find_hdds vmx vmx_source
            get_nvme_controller_target is_nvme_controller_target
            nvme_device_types nvme_controller

(* Find all SATA hard disks.
 *
 * In the VMX file:
 *   sata0.pciSlotNumber = "33"
 *   sata0:3.fileName = "win2019_1.vmdk"
 *
 * The "deviceType" field must be absent or "disk".  Other types here
 * include "cdrom-raw" and others indicating a CD-ROM, which we should
 * ignore.
 *)
and find_sata_disks vmx vmx_source =
  let get_sata_controller_target ns =
    sscanf ns "sata%d:%d" (fun c t -> c, t)
  in
  let is_sata_controller_target ns =
    try ignore (get_sata_controller_target ns); true
    with Scanf.Scan_failure _ | End_of_file | Failure _ -> false
  in
  let sata_device_types = [ Some "disk"; None ] in
  let sata_controller = Source_SATA in

  find_hdds vmx vmx_source
            get_sata_controller_target is_sata_controller_target
            sata_device_types sata_controller

(* Find all IDE hard disks.
 *
 * In the VMX file:
 *   ide0:0.deviceType = "ata-hardDisk"
 *   ide0:0.fileName = "guest.vmdk"
 *)
and find_ide_disks vmx vmx_source =
  let get_ide_controller_target ns =
    sscanf ns "ide%d:%d" (fun c t -> c, t)
  in
  let is_ide_controller_target ns =
    try ignore (get_ide_controller_target ns); true
    with Scanf.Scan_failure _ | End_of_file | Failure _ -> false
  in
  let ide_device_types = [ Some "ata-harddisk" ] in
  let ide_controller = Source_IDE in

  find_hdds vmx vmx_source
            get_ide_controller_target is_ide_controller_target
            ide_device_types ide_controller

and find_hdds vmx vmx_source
              get_controller_target is_controller_target
              device_types controller =
  (* Find namespaces matching '(ide|scsi|nvme|sata)X:Y' with suitable
   * deviceType.
   *)
  let hdds =
    Parse_vmx.select_namespaces (
      function
      | [ns] ->
         (* Check the namespace is '(ide|scsi|nvme|sata)X:Y' *)
         if not (is_controller_target ns) then false
         else (
           (* Check the deviceType is one we are looking for. *)
           let dt = Parse_vmx.get_string vmx [ns; "deviceType"] in
           let dt = Option.map String.lowercase_ascii dt in
           List.mem dt device_types
         )
      | _ -> false
    ) vmx in

  (* Map the subset to a list of disks. *)
  let hdds =
    Parse_vmx.map (
      fun path v ->
        match path, v with
        | [ns; "filename"], Some filename ->
           let c, t = get_controller_target ns in
           let s = { s_disk_id = (-1); s_controller = Some controller } in
           Some (c, t, s, filename)
        | _ -> None
    ) hdds in
  let hdds = List.filter_map Fun.id hdds in

  (* We don't have a way to return the controllers and targets, so
   * just make sure the disks are sorted into order, since Parse_vmx
   * won't return them in any particular order.
   *)
  let hdds = List.sort compare hdds in
  List.map (fun (_, _, source, filename) -> source, filename) hdds

(* Find all removable disks.
 *
 * In the VMX file:
 *   ide1:0.deviceType = "cdrom-image"
 *   ide1:0.fileName = "boot.iso"
 *
 * XXX This only supports IDE CD-ROMs, but we could support SCSI CD-ROMs, SATA
 * CD-ROMs, and floppies in future.
 *)
and find_removables vmx =
  let get_ide_controller_target ns =
    sscanf ns "ide%d:%d" (fun c t -> c, t)
  in
  let is_ide_controller_target ns =
    try ignore (get_ide_controller_target ns); true
    with Scanf.Scan_failure _ | End_of_file | Failure _ -> false
  in
  let device_types = [ "atapi-cdrom";
                       "cdrom-image"; "cdrom-raw" ] in

  (* Find namespaces matching 'ideX:Y' with suitable deviceType. *)
  let devs =
    Parse_vmx.select_namespaces (
      function
      | [ns] ->
         (* Check the namespace is 'ideX:Y' *)
         if not (is_ide_controller_target ns) then false
         else (
           (* Check the deviceType is one we are looking for. *)
           match Parse_vmx.get_string vmx [ns; "deviceType"] with
           | Some str ->
              let str = String.lowercase_ascii str in
              List.mem str device_types
           | None -> false
         )
      | _ -> false
    ) vmx in

  (* Map the subset to a list of CD-ROMs. *)
  let devs =
    Parse_vmx.map (
      fun path v ->
        match path, v with
        | [ns], None ->
           let c, t = get_ide_controller_target ns in
           let s = { s_removable_type = CDROM;
                     s_removable_controller = Some Source_IDE;
                     s_removable_slot = Some (ide_slot c t) } in
           Some s
        | _ -> None
    ) devs in
  let devs = List.filter_map Fun.id devs in

  (* Sort by slot. *)
  let devs =
    List.sort
      (fun { s_removable_slot = s1 } { s_removable_slot = s2 } ->
        compare s1 s2)
      devs in

  devs

and ide_slot c t =
  (* Assuming the old master/slave arrangement. *)
  c * 2 + t

(* Find all ethernet cards.
 *
 * In the VMX file:
 *   ethernet0.virtualDev = "vmxnet3"
 *   ethernet0.networkName = "VM Network"
 *   ethernet0.generatedAddress = "00:01:02:03:04:05"
 *   ethernet0.connectionType = "bridged" # also: "custom", "nat" or not present
 *)
and find_nics vmx =
  let get_ethernet_port ns =
    sscanf ns "ethernet%d" (fun p -> p)
  in
  let is_ethernet_port ns =
    try ignore (get_ethernet_port ns); true
    with Scanf.Scan_failure _ | End_of_file | Failure _ -> false
  in

  (* Find namespaces matching 'ethernetX'. *)
  let nics =
    Parse_vmx.select_namespaces (
      function
      | [ns] -> is_ethernet_port ns
      | _ -> false
    ) vmx in

  (* Map the subset to a list of NICs. *)
  let nics =
    Parse_vmx.map (
      fun path v ->
        match path, v with
        | [ns], None ->
           let port = get_ethernet_port ns in
           let mac = Parse_vmx.get_string vmx [ns; "generatedAddress"] in
           let model = Parse_vmx.get_string vmx [ns; "virtualDev"] in
           let model =
             match model with
             | Some m when String.lowercase_ascii m = "e1000" ->
                Some Source_e1000
             | Some model ->
                Some (Source_other_nic (String.lowercase_ascii model))
             | None -> None in
           let vnet = Parse_vmx.get_string vmx [ns; "networkName"] in
           let vnet =
             match vnet with
             | Some vnet -> vnet
             | None -> ns (* "ethernetX" *) in
           let vnet_type =
             match Parse_vmx.get_string vmx [ns; "connectionType"] with
             | Some b when String.lowercase_ascii b = "bridged" ->
                Bridge
             | Some _ | None -> Network in
           Some (port,
                 { s_mac = mac; s_nic_model = model;
                   s_vnet = vnet;
                   s_vnet_type = vnet_type })
        | _ -> None
    ) nics in
  let nics = List.filter_map Fun.id nics in

  (* Sort by port. *)
  let nics = List.sort compare nics in

  let nics = List.map (fun (_, source) -> source) nics in
  nics

let parse_domain_from_vmx vmx_source =
  let tmpdir =
    let t = Mkdtemp.temp_dir "vmx." in
    On_exit.rm_rf t;
    t in

  (* If the transport is SSH, fetch the file from remote, else
   * parse it from local.
   *)
  let vmx =
    match vmx_source with
    | VMXSourceFile filename -> Parse_vmx.parse_file filename
    | VMXSourceSSH (password, uri) ->
       let server =
         match uri.uri_server with
         | None -> assert false (* checked by vmx_source_of_arg *)
         | Some server -> server in
       let port =
         match uri.uri_port with
         | i when i <= 0 -> None
         | i -> Some (string_of_int i) in
       let user = uri.Xml.uri_user in
       let path =
         match uri.uri_path with
         | None -> assert false (* checked by vmx_source_of_arg *)
         | Some path -> path in
       let filename = tmpdir // "source.vmx" in
       Ssh.download_file ?password ?port ~server ?user path filename;
       Parse_vmx.parse_file filename in

  let name =
    match Parse_vmx.get_string vmx ["displayName"] with
    | Some s -> s
    | None ->
       warning (f_"no displayName key found in VMX file");
       match vmx_source with
       | VMXSourceFile filename -> name_from_disk filename
       | VMXSourceSSH (_, uri) ->
          match uri.uri_path with
          | None -> assert false (* checked by vmx_source_of_arg *)
          | Some path -> name_from_disk path in

  let genid =
    (* See: https://lists.nongnu.org/archive/html/qemu-devel/2018-07/msg02019.html *)
    let genid_lo = Parse_vmx.get_int64 vmx ["vm"; "genid"]
    and genid_hi = Parse_vmx.get_int64 vmx ["vm"; "genidX"] in
    match genid_lo, genid_hi with
    | None, None | Some _, None | None, Some _ ->
       None
    | Some lo, Some hi ->
       (* See docs/vm-generation-id-across-hypervisors.txt *)
       let sub = String.sub (sprintf "%016Lx%016Lx" lo hi) in
       let uuid =
         sub  8 8 ^ "-" ^
         sub  4 4 ^ "-" ^
         sub  0 4 ^ "-" ^
         sub 30 2 ^ sub 28 2 ^ "-" ^
         sub 26 2 ^ sub 24 2 ^ sub 22 2 ^ sub 20 2 ^ sub 18 2 ^ sub 16 2 in
       Some uuid in

  let memory_mb =
    match Parse_vmx.get_int64 vmx ["memSize"] with
    | None -> 32_L            (* default is really 32 MB! *)
    | Some i -> i in
  let memory = memory_mb *^ 1024L *^ 1024L in

  let vcpu =
    match Parse_vmx.get_int vmx ["numvcpus"] with
    | None -> 1
    | Some i -> i in

  let cpu_topology =
    match Parse_vmx.get_int vmx ["cpuid"; "coresPerSocket"] with
    | None -> None
    | Some cores_per_socket ->
       let sockets = vcpu / cores_per_socket in
       if sockets <= 0 then (
         warning (f_"invalid cpuid.coresPerSocket < number of vCPUs");
         None
       )
       else
         Some { s_cpu_sockets = sockets; s_cpu_cores = cores_per_socket;
                s_cpu_threads = 1 } in

  let firmware =
    match Parse_vmx.get_string vmx ["firmware"] with
    | None -> BIOS
    | Some "efi" -> UEFI
    (* Other values are not documented for this field ... *)
    | Some fw ->
       warning (f_"unknown firmware value '%s', assuming BIOS") fw;
       BIOS in

  let uefi_secureboot =
    Parse_vmx.get_bool vmx ["uefi"; "secureBoot"; "enabled"] |>
      Option.value ~default:false in

  let sound =
    match Parse_vmx.get_string vmx ["sound"; "virtualDev"] with
    | Some "sb16" -> Some { s_sound_model = SB16 }
    | Some "es1371" -> Some { s_sound_model = ES1370 (* hmmm ... *) }
    | Some "hdaudio" -> Some { s_sound_model = ICH6 (* intel-hda *) }
    | Some model ->
       warning (f_"unknown sound device '%s' ignored") model;
       None
    | None -> None in

  let disks = find_disks vmx vmx_source in
  let removables = find_removables vmx in
  let nics = find_nics vmx in

  let source = {
    s_hypervisor = VMware;
    s_name = name;
    s_genid = genid;
    s_memory = memory;
    s_vcpu = vcpu;
    s_cpu_vendor = None;
    s_cpu_model = None;
    s_cpu_topology = cpu_topology;
    s_features = [];
    s_firmware = firmware;
    s_uefi_secureboot = uefi_secureboot;
    s_display = None;
    s_sound = sound;
    s_disks = List.map fst disks;
    s_removables = removables;
    s_nics = nics;
  } in

  source, List.map snd disks
