(* virt-v2v
 * Copyright (C) 2009-2020 Red Hat Inc.
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

open C_utils
open Std_utils
open Tools_utils
open Common_gettext.Gettext
open Xpath_helpers

open Types
open Utils

type disk = {
  d_format : string option;     (* Disk format from XML if known. *)
  d_type : disk_type;           (* Disk type and extra information. *)
  d_checksum : disk_checksum option; (* Disk checksum, if present. *)
}
and disk_type =
  | BlockDev of string          (* type=block with <source dev=...> *)
  | LocalFile of string         (* type=file with <source file=...> *)
  | NBD of string * int         (* NBD forward to hostname:port *)
  | HTTP of string              (* HTTP/HTTPS URL *)
and disk_checksum = {
  checksum_method : string;     (* Checksum method, eg. "sha256". *)
  checksum_expected : string;   (* Expected checksum. *)
  checksum_on_fail : checksum_on_fail
}
and checksum_on_fail = ChecksumOnFailWarn | ChecksumOnFailError | ChecksumPrint

(* Turn string like "hda" into controller slot number.  See also
 * common/utils/utils.c:guestfs_int_drive_index which this function calls.
 *)
let get_drive_slot str offset =
  let name = String.sub str offset (String.length str - offset) in
  try Some (drive_index name)
  with Invalid_argument _ ->
       warning (f_"could not parse device name ‘%s’ \
                   from the source libvirt XML") str;
       None

let parse_libvirt_xml ?conn xml =
  debug "libvirt xml is:\n%s" xml;

  (* Create a default libvirt connection on request, to not open one
   * in case there is no need to fetch more data (for example inspect
   * the storage pools).
   *)
  let libvirt_conn =
    lazy (Libvirt.Connect.connect ()) in
  let get_conn () =
    match conn with
    | None -> Lazy.force libvirt_conn
    | Some conn -> conn in

  let doc = Xml.parse_memory xml in
  let xpathctx = Xml.xpath_new_context doc in
  let xpath_string = xpath_string xpathctx
  and xpath_int = xpath_int xpathctx
  and xpath_int64 = xpath_int64 xpathctx in

  let hypervisor =
    match xpath_string "/domain/@type" with
    | None | Some "" ->
       error (f_"in the libvirt XML metadata, <domain type='...'> \
                 is missing or empty")
    | Some s -> source_hypervisor_of_string s in
  let name =
    match xpath_string "/domain/name/text()" with
    | None | Some "" ->
       error (f_"in the libvirt XML metadata, <name> is missing or empty")
    | Some s -> String.trim s in
  let genid =
    match xpath_string "/domain/genid/text()" with
    | None | Some "" -> None
    | Some _ as s -> s in
  let memory =
    Option.value ~default:(1024L *^ 1024L) (xpath_int64 "/domain/memory/text()") in
  let memory = memory *^ 1024L in

  let cpu_vendor = xpath_string "/domain/cpu/vendor/text()" in
  let cpu_model = xpath_string "/domain/cpu/model/text()" in
  let cpu_sockets = xpath_int "/domain/cpu/topology/@sockets" in
  let cpu_cores = xpath_int "/domain/cpu/topology/@cores" in
  let cpu_threads = xpath_int "/domain/cpu/topology/@threads" in

  (* Get the <vcpu> field from the input XML.  If not set then
   * try calculating it from the <cpu> <topology> node.  If that's
   * not set either, then assume 1 vCPU.
   *)
  let vcpu = xpath_int "/domain/vcpu/text()" in
  let vcpu =
    match vcpu, cpu_sockets, cpu_cores, cpu_threads with
    | Some vcpu, _,    _,    _    -> vcpu
    | None,      None, None, None -> 1
    | None,      _,    _,    _    ->
       let sockets = Option.value ~default:1 cpu_sockets
       and cores = Option.value ~default:1 cpu_cores
       and threads = Option.value ~default:1 cpu_threads in
       sockets * cores * threads in

  let cpu_topology =
    match cpu_sockets, cpu_cores, cpu_threads with
    | Some sockets, Some cores, Some threads ->
       Some { s_cpu_sockets = sockets; s_cpu_cores = cores;
              s_cpu_threads = threads; }
    | _, _, _ -> None in

  let features =
    let nodes = xpath_get_nodes xpathctx "/domain/features/*" in
    List.map Xml.node_name nodes in

  let display =
    let obj = Xml.xpath_eval_expression xpathctx "/domain/devices/graphics" in
    let nr_nodes = Xml.xpathobj_nr_nodes obj in
    if nr_nodes < 1 then None
    else (
      (* Ignore everything except the first <graphics> device. *)
      let node = Xml.xpathobj_node obj 0 in
      Xml.xpathctx_set_current_context xpathctx node;
      let keymap = xpath_string "@keymap" in
      let password = xpath_string "@passwd" in
      let listen =
        let obj = Xml.xpath_eval_expression xpathctx "listen" in
        let nr_nodes = Xml.xpathobj_nr_nodes obj in
        if nr_nodes < 1 then (
          match xpath_string "@listen" with
          | None -> LNoListen | Some a -> LAddress a
        ) else (
          (* Use only the first <listen> configuration. *)
          match xpath_string "listen[1]/@type" with
          | None -> LNoListen
          | Some "address" ->
            (match xpath_string "listen[1]/@address" with
            | None -> LNoListen
            | Some a -> LAddress a
            )
          | Some "network" ->
            (match xpath_string "listen[1]/@network" with
            | None -> LNoListen
            | Some n -> LNetwork n
            )
          | Some "socket" ->
            (match xpath_string "listen[1]/@socket" with
            | None -> LSocket None
            | Some n -> LSocket (Some n)
            )
          | Some "none" ->
            LNone
          | Some t ->
            warning (f_"<listen type='%s'> in the input libvirt XML \
                        was ignored") t;
            LNoListen
        ) in
      let port =
        match xpath_string "@autoport" with
        | Some "no" ->
          (match xpath_int "@port" with
           | Some port when port > 0 -> Some port
           | Some _ | None -> None)
        | _ -> None in
      match xpath_string "@type" with
      | None -> None
      | Some "vnc" ->
        Some { s_display_type = VNC;
               s_keymap = keymap; s_password = password; s_listen = listen;
               s_port = port }
      | Some "spice" ->
        Some { s_display_type = Spice;
               s_keymap = keymap; s_password = password; s_listen = listen;
               s_port = port }
      | Some ("sdl"|"desktop" as t) ->
        warning (f_"virt-v2v does not support local displays, so \
                    <graphics type='%s'> in the input libvirt XML was ignored")
          t;
        None
      | Some t ->
        warning (f_"display <graphics type='%s'> in the input \
                    libvirt XML was ignored") t;
        None
    ) in

  (* Sound card. *)
  let sound =
    let obj = Xml.xpath_eval_expression xpathctx "/domain/devices/sound" in
    let nr_nodes = Xml.xpathobj_nr_nodes obj in
    if nr_nodes < 1 then None
    else (
      (* Ignore everything except the first <sound> device. *)
      let node = Xml.xpathobj_node obj 0 in

      Xml.xpathctx_set_current_context xpathctx node;
      match xpath_string "@model" with
      | None -> None
      | Some model ->
         match source_sound_model_of_string model with
         | Some s_sound_model -> Some { s_sound_model }
         | None ->
            warning (f_"unknown sound model %s ignored") model;
            None
    ) in

  (* Presence of virtio-scsi controller. *)
  let has_virtio_scsi =
    let obj = Xml.xpath_eval_expression xpathctx
                "/domain/devices/controller[@model='virtio-scsi']" in
    Xml.xpathobj_nr_nodes obj > 0 in

  (* Non-removable disk devices. *)
  let s_disks, disks =
    let get_disks, add_disk =
      let s_disks = ref [] and disks = ref [] and i = ref (-1) in
      let get_disks () = List.rev !s_disks, List.rev !disks in
      let add_disk format controller typ checksum =
        incr i;
        let s_disk = { s_disk_id = !i; s_controller = controller }
        and disk = {
          d_format = format;
          d_type = typ;
          d_checksum = checksum;
        } in
        List.push_front s_disk s_disks;
        List.push_front disk disks
      in
      get_disks, add_disk
    in
    let obj =
      Xml.xpath_eval_expression xpathctx
        "/domain/devices/disk[@device='disk']" in
    let nr_nodes = Xml.xpathobj_nr_nodes obj in
    if nr_nodes < 1 then
      error (f_"this guest has no non-removable disks");
    for i = 0 to nr_nodes-1 do
      let node = Xml.xpathobj_node obj i in
      Xml.xpathctx_set_current_context xpathctx node;

      let controller =
        let target_bus = xpath_string "target/@bus" in
        match target_bus, has_virtio_scsi with
        | None, _ -> None
        | Some "ide", _ -> Some Source_IDE
        | Some "sata", _ -> Some Source_SATA
        | Some "scsi", true -> Some Source_virtio_SCSI
        | Some "scsi", false -> Some Source_SCSI
        | Some "virtio", _ -> Some Source_virtio_blk
        | Some _, _ -> None in

      let format =
        match xpath_string "driver/@type" with
        | Some "aio" -> Some "raw" (* Xen wierdness *)
        | None -> None
        | Some format -> Some format in

      let checksum =
        match xpath_string "checksum/text()",
              xpath_string "checksum/@method",
              xpath_string "checksum/@fail" with
        | _, Some meth, Some ("print") ->
           Some { checksum_expected = ""; checksum_method = meth;
                  checksum_on_fail = ChecksumPrint }
        | None, _, _ -> None
        | Some csum, None, _ ->
           warning (f_"<checksum> missing 'method' attribute, ignoring");
           None
        | _, _, (None | Some "ignore") -> None
        | Some csum, Some meth, Some ("warn" | "warning") ->
           Some { checksum_expected = String.trim csum;
                  checksum_method = meth;
                  checksum_on_fail = ChecksumOnFailWarn }
        | Some csum, Some meth, Some ("err" | "error") ->
           Some { checksum_expected = String.trim csum;
                  checksum_method = meth;
                  checksum_on_fail = ChecksumOnFailError }
        | _, _, Some v ->
           warning (f_"<checksum> unknown fail='%s' attribute, ignoring") v;
           None in

      (* The <disk type='...'> attribute may be 'block', 'file',
       * 'network' or 'volume'.  We ignore any other types.
       *)
      match xpath_string "@type" with
      | None ->
         warning (f_"<disk> element with no type attribute ignored")
      | Some "block" ->
        (match xpath_string "source/@dev" with
         | Some path ->
            add_disk format controller (BlockDev path) checksum
         | None -> ()
        );
      | Some "file" ->
        (match xpath_string "source/@file" with
         | Some path ->
            add_disk format controller (LocalFile path) checksum
         | None -> ()
        );
      | Some "network" ->
        (match (xpath_string "source/@protocol",
                xpath_string "source/host/@name",
                xpath_int "source/host/@port") with
        | None, _, _ ->
           warning (f_"<disk type='%s'> was ignored") "network"
        | Some "nbd", Some ("localhost" as host), Some port when port > 0 ->
           (* <source protocol="nbd"> with host localhost is used by virt-p2v *)
           add_disk format controller (NBD (host, port)) checksum
        | Some ("http"|"https" as driver), Some (_ as host), port ->
           (* This is for testing curl, eg for testing VMware conversions
            * without needing VMware around.
            *)
           let path = Option.value ~default:"" (xpath_string "source/@name") in
           let url =
             let port =
               match driver, port with
               | _, None -> ""
               | "https", Some 443 -> ""
               | "http", Some 80 -> ""
               | _, Some port when port >= 1 -> ":" ^ string_of_int port
               | _, Some port ->
                  invalid_arg "invalid port number in libvirt XML" in
             sprintf "%s://%s%s%s" driver host port (uri_quote path) in
           add_disk format controller (HTTP url) checksum
        | Some protocol, _, _ ->
           warning (f_"<disk type='network'> with <source protocol='%s'> \
                       was ignored")
                   protocol
        )
      | Some "volume" ->
        (match xpath_string "source/@pool", xpath_string "source/@volume" with
        | None, None | Some _, None | None, Some _ -> ()
        | Some pool, Some vol ->
          let xml =
            let pool = Libvirt_utils.get_pool (get_conn ()) pool in
            let vol = Libvirt_utils.get_volume pool vol in
            Libvirt.Volume.get_xml_desc (Libvirt.Volume.const vol) in
          let doc = Xml.parse_memory xml in
          let xpathctx = Xml.xpath_new_context doc in
          let xpath_string = Xpath_helpers.xpath_string xpathctx in

          (* Use the format specified in the volume itself. *)
          let format = xpath_string "/volume/target/format/@type" in

          (match xpath_string "/volume/@type" with
          | None | Some "file" ->
            (match xpath_string "/volume/target/path/text()" with
             | Some path ->
                add_disk format controller (LocalFile path) checksum
             | None -> ()
            );
          | Some vol_type ->
            warning (f_"<disk type='volume'> with <volume type='%s'> \
                        was ignored") vol_type
          )
        )
      | Some disk_type ->
        warning (f_"<disk type='%s'> was ignored") disk_type
    done;
    get_disks () in

  (* Removable devices, CD-ROMs and floppy disks. *)
  let removables =
    let obj =
      Xml.xpath_eval_expression xpathctx
        "/domain/devices/disk[@device='cdrom' or @device='floppy']" in
    let nr_nodes = Xml.xpathobj_nr_nodes obj in
    let disks = ref [] in
    for i = 0 to nr_nodes-1 do
      let node = Xml.xpathobj_node obj i in
      Xml.xpathctx_set_current_context xpathctx node;

      let controller =
        let target_bus = xpath_string "target/@bus" in
        match target_bus, has_virtio_scsi with
        | None, _ -> None
        | Some "ide", _ -> Some Source_IDE
        | Some "sata", _ -> Some Source_SATA
        | Some "scsi", true -> Some Source_virtio_SCSI
        | Some "scsi", false -> Some Source_SCSI
        | Some "virtio", _ -> Some Source_virtio_blk
        | Some _, _ -> None in

      let slot =
        let target_dev = xpath_string "target/@dev" in
        match target_dev with
        | None -> None
        | Some dev when String.is_prefix dev "sr" ->
           (* "srN" devices are found mostly in the physical XML written by
            * virt-p2v.
            *)
           let name = String.sub dev 2 (String.length dev - 2) in
           (try Some (int_of_string name)
            with Failure _ ->
              warning (f_"could not parse device name ‘%s’ \
                          from the source libvirt XML") dev;
              None
           )
        | Some dev ->
           let rec loop = function
             | [] ->
                warning (f_"<target dev='%s'> was ignored because \
                            the device name could not be recognized") dev;
                None
             | prefix :: rest ->
                if String.is_prefix dev prefix then (
                  let offset = String.length prefix in
                  get_drive_slot dev offset
                )
                else
                  loop rest
           in
           loop ["hd"; "sd"; "vd"; "xvd"; "fd"] in

      let typ =
        match xpath_string "@device" with
        | Some "cdrom" -> CDROM
        | Some "floppy" -> Floppy
        | _ -> assert false (* libxml2 error? *) in

      let disk =
        { s_removable_type = typ;
          s_removable_controller = controller;
          s_removable_slot = slot } in
      List.push_front disk disks
    done;
    List.rev !disks in

  (* Network interfaces. *)
  let nics =
    let obj = Xml.xpath_eval_expression xpathctx "/domain/devices/interface" in
    let nr_nodes = Xml.xpathobj_nr_nodes obj in
    let nics = ref [] in
    for i = 0 to nr_nodes-1 do
      let node = Xml.xpathobj_node obj i in
      Xml.xpathctx_set_current_context xpathctx node;

      let mac = xpath_string "mac/@address" in
      let mac =
        match mac with
        | None
        | Some "00:00:00:00:00:00" (* thanks, VMware *) -> None
        | Some mac -> Some mac in

      let model =
        match xpath_string "model/@type" with
        | None -> None
        | Some model -> Some (nic_model_of_string model) in

      let vnet_type =
        match xpath_string "@type" with
        | Some "network" -> Some Network
        | Some "bridge" -> Some Bridge
        | None | Some _ -> None in
      match vnet_type with
      | None -> ()
      | Some vnet_type ->
         let add_nic vnet =
           let nic = {
             s_mac = mac;
             s_nic_model = model;
             s_vnet = vnet;
             s_vnet_type = vnet_type
           } in
           List.push_front nic nics
         in
         match xpath_string "source/@network | source/@bridge" with
         | None -> ()
         | Some "" ->
            (* The libvirt VMware driver produces at least <source
             * bridge=''/> XML - see RHBZ#1257895.
             *)
            add_nic (sprintf "eth%d" i)
         | Some vnet ->
            add_nic vnet
    done;
    List.rev !nics in

  (* Firmware.
   * If "/domain/os" node doesn't contain "firmware" attribute (automatic
   * firmware), we look for the presence of "pflash" in
   * "/domain/os/loader/@type" attribute (manual firmware), with the latter
   * indicating the UEFI firmware.
   * See https://libvirt.org/formatdomain.html#bios-bootloader
   *)
  let firmware =
    match xpath_string "/domain/os/@firmware" with
    | Some "bios" -> BIOS
    | Some "efi" -> UEFI
    | Some _ -> UnknownFirmware
    | None -> (
        match xpath_string "/domain/os/loader/@type" with
        | Some "pflash" -> UEFI
        | _ -> UnknownFirmware
      ) in

  (* Check for hostdev devices. (RHBZ#1472719) *)
  let () =
    let obj = Xml.xpath_eval_expression xpathctx "/domain/devices/hostdev" in
    let nr_nodes = Xml.xpathobj_nr_nodes obj in
    if nr_nodes > 0 then (
      (* Sadly fn_ in ocaml-gettext seems broken, and always returns the
       * singular string no matter what.  Work around this by using a simple
       * string with sn_ (which works), and outputting it as a whole.
       *)
      let msg = sn_ "this guest has a passthrough host device which will be ignored"
                    "this guest has passthrough host devices which will be ignored"
                    nr_nodes in
      warning "%s" msg
    )
  in

  (* Check for direct attachments to physical network interfaces.
   * (RHBZ#1518539)
   *)
  let () =
    let obj = Xml.xpath_eval_expression xpathctx "/domain/devices/interface[@type='direct']" in
    let nr_nodes = Xml.xpathobj_nr_nodes obj in
    if nr_nodes > 0 then (
      (* Sadly fn_ in ocaml-gettext seems broken, and always returns the
       * singular string no matter what.  Work around this by using a simple
       * string with sn_ (which works), and outputting it as a whole.
       *)
      let msg = sn_ "this guest has a direct network interface which will be ignored"
                    "this guest has direct network interfaces which will be ignored"
                    nr_nodes in
      warning "%s" msg
    )
  in

  ({
    s_hypervisor = hypervisor;
    s_name = name;
    s_genid = genid;
    s_memory = memory;
    s_vcpu = vcpu;
    s_cpu_vendor = cpu_vendor;
    s_cpu_model = cpu_model;
    s_cpu_topology = cpu_topology;
    s_features = features;
    s_firmware = firmware;
    s_uefi_secureboot = false;
    s_display = display;
    s_sound = sound;
    s_disks = s_disks;
    s_removables = removables;
    s_nics = nics;
   },
   disks)

let parse_libvirt_domain conn guest =
  let dom = Libvirt_utils.get_domain conn guest in
  (* Use XmlSecure to get passwords (RHBZ#1174123). *)
  let xml = Libvirt.Domain.get_xml_desc_flags dom [Libvirt.Domain.XmlSecure] in
  let source, disks = parse_libvirt_xml ~conn xml in
  source, disks, xml
