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
open C_utils
open Tools_utils
open Common_gettext.Gettext

open Types
open Utils
open DOM

let string_set_of_list =
  List.fold_left (fun set x -> StringSet.add x set) StringSet.empty

let get_osinfo_id inspect =
  match Libosinfo_utils.get_os_by_short_id inspect.i_osinfo
  with
  | Some os ->
    Some(os#get_id ())
  | None ->
    warning (f_"get_osinfo_id: unknown guest operating system: %s")
      inspect.i_osinfo;
    None

let create_libvirt_xml ?pool source inspect
      { guestcaps; target_buses; target_nics; target_firmware;
        target_boot_device }
      target_features domcaps_features outdisk_name output_format output_name =
  debug "info: domcaps: %s" (string_of_domcaps domcaps_features);

  (* The main body of the libvirt XML document. *)
  let body = ref [] in

  List.push_back_list body [
    Comment generated_by;
    e "name" [] [PCData output_name];
  ];

  (match source.s_genid with
   | None -> ()
   | Some genid -> List.push_back body (e "genid" [] [PCData genid])
  );

  (match get_osinfo_id inspect with
   | None -> ()
   | Some osinfo_id ->
     List.push_back_list body [
       e "metadata" [] [
         e "libosinfo:libosinfo" ["xmlns:libosinfo", "http://libosinfo.org/xmlns/libvirt/domain/1.0"] [
           e "libosinfo:os" ["id", osinfo_id] [];
         ];
       ];
     ];
  );

  let memory_k = source.s_memory /^ 1024L in
  List.push_back_list body [
    e "memory" ["unit", "KiB"] [PCData (Int64.to_string memory_k)];
    e "currentMemory" ["unit", "KiB"] [PCData (Int64.to_string memory_k)];
    e "vcpu" [] [PCData (string_of_int source.s_vcpu)]
  ];

  let cpu_attrs = ref []
  and cpu = ref [] in

  (match source.s_cpu_model with
   | None ->
      List.push_back cpu_attrs ("mode", "host-model");
   | Some model ->
      List.push_back cpu_attrs ("match", "minimum");
      if model = "qemu64" then
        List.push_back cpu_attrs ("check", "none");
      (match source.s_cpu_vendor with
       | None -> ()
       | Some vendor ->
          List.push_back cpu (e "vendor" [] [PCData vendor])
      );
      List.push_back cpu (e "model" ["fallback", "allow"] [PCData model])
  );
  (match source.s_cpu_topology with
   | None -> ()
   | Some { s_cpu_sockets; s_cpu_cores; s_cpu_threads } ->
      let topology_attrs = [
        "sockets", string_of_int s_cpu_sockets;
        "cores", string_of_int s_cpu_cores;
        "threads", string_of_int s_cpu_threads;
      ] in
      List.push_back cpu (e "topology" topology_attrs [])
  );

  List.push_back_list body [ e "cpu" !cpu_attrs !cpu ];

  (* We have the machine features of the guest when it was on the
   * source hypervisor (source.s_features).  We have the set of
   * hypervisor features supported by the target (target_features).
   * Combine these into a final list of features.
   *)
  let features = string_set_of_list source.s_features in
  let features = StringSet.add "acpi" features in
  let target_features = string_set_of_list target_features in

  (* Make sure we don't add any features which are not supported by
   * the target hypervisor.
   *)
  let features = StringSet.inter(*section*) features target_features in

  (* But if the target supports apic or pae then we should add them
   * anyway (old virt-v2v did this).
   *)
  let force_features = string_set_of_list ["apic"; "pae"] in
  let force_features =
    StringSet.inter(*section*) force_features target_features in
  let features = StringSet.union features force_features in

  let features = List.sort compare (StringSet.elements features) in

  List.push_back_list body [
    e "features" [] (List.map (fun s -> e s [] []) features);
  ];

  (* The <os> section subelements. *)
  let os_section =
    let os = ref [] in

    let firmware_attribute =
      match target_firmware with
      | TargetBIOS -> []
      | TargetUEFI -> [ "firmware", "efi" ] in

    let () =
      match target_firmware with
      | TargetBIOS -> ()
      | TargetUEFI ->
         (* The UEFI secureboot setting is sort of adjacent to if
          * the full secure boot feature is enabled, but it's what
          * we have.
          * https://libvirt.org/kbase/secureboot.html
          *)
         let sb = source.s_uefi_secureboot in
         let enrolled_keys =
           if sb then
             [e "feature" ["name", "enrolled-keys"; "enabled", "yes"] []]
           else [] in
         let firmware_features =
           (e "feature" ["name", "secure-boot";
                         "enabled", if sb then "yes" else "no"] []) ::
             enrolled_keys in
         List.push_back os (e "firmware" [] firmware_features) in

    let machine =
      match guestcaps.gcaps_machine with
      | I440FX -> "pc"
      | Q35 -> "q35"
      | Virt -> "virt" in

    List.push_back os
                   (e "type" ["arch", guestcaps.gcaps_arch;
                              "machine", machine]
                      [PCData "hvm"]);

    e "os" firmware_attribute !os in

  (* The <clock> section. *)
  let clock_section =
    let offset = if guestcaps.gcaps_rtc_utc then "utc" else "localtime" in
    e "clock" [ "offset", offset ] [] in

  List.push_back_list body [
    os_section;
    clock_section;

    e "on_poweroff" [] [PCData "destroy"];
    e "on_reboot" [] [PCData "restart"];
    e "on_crash" [] [PCData "restart"];
  ];

  (* The devices. *)
  let devices = ref [] in

  (* This will affect all of the virtio devices (if any). *)
  let virtio_transitional =
    guestcaps.gcaps_machine = Q35 && not guestcaps.gcaps_virtio_1_0 in
  let virtio_model =
    if virtio_transitional then "virtio-transitional" else "virtio" in

  (* Fixed and removable disks. *)
  let () =
    (* Track per-prefix dev indices (sd, vd, hd, fd, ...) so that we never
     * re-use the same target dev string for multiple <disk> elements.
     *
     * This avoids collisions when multiple buses share a prefix, e.g.:
     *   - Q35: SATA and SCSI both using "sd".
     *)
    let dev_counters : (string, int ref) Hashtbl.t = Hashtbl.create 7 in

    let next_dev drive_prefix =
      let idx =
        try
          let r = Hashtbl.find dev_counters drive_prefix in
          let i = !r in
          incr r;
          i
        with Not_found ->
          (* First allocation for this prefix. *)
          Hashtbl.add dev_counters drive_prefix (ref 1);
          0
      in
      drive_prefix ^ drive_name idx
    in

    let make_disk bus_name ?(viotrans = false) drive_prefix i = function
    | BusSlotEmpty -> Comment (sprintf "%s slot %d is empty" bus_name i)

    | BusSlotDisk d ->
       let outdisk = outdisk_name d.s_disk_id in

       let boot_order =
         match target_boot_device with
         | None ->
            (* No known boot device, just number them sequentially. *)
            i+1
         | Some disk_index when disk_index = i ->
            (* For the boot disk, use order 1. *)
            1
         | Some _ ->
            (* For the others number them sequentially starting at 2. *)
            i+2 in

        let dev = next_dev drive_prefix in

        e "disk" (
          [
            "type", if pool = None then "file" else "volume";
            "device", "disk"
          ] @ if (viotrans) then [ "model", "virtio-transitional" ] else []
        ) [
          e "driver" [
            "name", "qemu";
            "type", output_format;
          ] [];
          (match pool with
          | None ->
            e "source" [
              "file", outdisk;
            ] []
          | Some pool ->
            e "source" [
              "pool", pool;
              "volume", Filename.basename outdisk;
            ] []
          );
          e "target" [
            "dev", dev;
            "bus", bus_name;
          ] [];
          e "boot" [ "order", string_of_int boot_order ] [];
        ]

    | BusSlotRemovable { s_removable_type = CDROM } ->
        let dev = next_dev drive_prefix in
        e "disk" [ "device", "cdrom"; "type", "file" ] [
          e "driver" [ "name", "qemu"; "type", "raw" ] [];
          e "target" [
            "dev", dev;
            "bus", bus_name
          ] []
        ]

    | BusSlotRemovable { s_removable_type = Floppy } ->
        let dev = next_dev drive_prefix in
        e "disk" [ "device", "floppy"; "type", "file" ] [
          e "driver" [ "name", "qemu"; "type", "raw" ] [];
          e "target" [
            "dev", dev;
          ] []
        ]
    in

    List.push_back_list devices
      (List.mapi (make_disk "virtio" ~viotrans:virtio_transitional "vd")
                 (Array.to_list target_buses.target_virtio_blk_bus));
    let ide_disks =
      match guestcaps.gcaps_machine with
      | I440FX ->
         List.mapi (make_disk "ide" "hd")
                   (Array.to_list target_buses.target_ide_bus)
      | Q35 ->
         List.mapi (make_disk "sata" "sd")
                   (Array.to_list target_buses.target_ide_bus)
      | Virt ->
         (* mach_virt doesn't support legacy devices like IDE and SATA,
          * so target_ide_bus must be empty, otherwise we give a warning.
          *)
         if Array.length target_buses.target_ide_bus > 0 then
           warning "machine type virt does not support IDE and SATA legacy \
                    devices, some legacy devices of this guest have been \
                    dropped from the libvirt output";
         [] in
    List.push_back_list devices ide_disks;
    List.push_back_list devices
      (List.mapi (make_disk "scsi" "sd")
                 (Array.to_list target_buses.target_scsi_bus));
    let floppy_devices = Array.to_list target_buses.target_floppy_bus in
    if not (List.for_all (function BusSlotEmpty -> true | _ -> false) floppy_devices) then (
      if not domcaps_features.supports_floppy then
        warning (f_"target hypervisor does not support floppy devices, \
                    but floppy devices were found in the source guest")
      else
        List.push_back_list devices
          (List.mapi (make_disk "floppy" "fd") floppy_devices)
    ) in

  let nics =
    let net_model =
      match guestcaps.gcaps_net_bus with
      | Virtio_net -> virtio_model | E1000 -> "e1000" | RTL8139 -> "rtl8139" in
    List.map (
      fun { s_mac = mac; s_vnet_type = vnet_type; s_vnet = vnet } ->
        let vnet_type_str =
          match vnet_type with
          | Bridge -> "bridge" | Network -> "network" in

        let nic =
          let children = [
            e "source" [ vnet_type_str, vnet ] [];
            e "model" [ "type", net_model ] [];
          ] in
          e "interface" [ "type", vnet_type_str ] children in

        (match mac with
        | None -> ()
        | Some mac ->
          append_child (e "mac" [ "address", mac ] []) nic);

        nic
    ) target_nics in
  List.push_back_list devices nics;

  (* Same as old virt-v2v, we always add a display here even if it was
   * missing from the old metadata.
   *)
  let video =
    let video_model =
      e "model" [ "type", "vga"; "vram", "16384" ] [] in
    append_attr ("heads", "1") video_model;
    e "video" [] [ video_model ] in
  List.push_back devices video;

  let graphics =
    match source.s_display with
    | None ->
       e "graphics" [ "type", "vnc"; "autoport", "yes" ] []
    | Some { s_display_type = Window } ->
       e "graphics" [ "type", "sdl" ] []
    | Some { s_display_type = VNC; s_port = Some p } ->
       e "graphics" [ "type", "vnc"; "port", string_of_int p ] []
    | Some { s_display_type = VNC; s_port = None } ->
       e "graphics" [ "type", "vnc"; "autoport", "yes" ] []
    | Some { s_display_type = Spice; s_port = Some p } ->
       e "graphics" [ "type", "spice"; "port", string_of_int p ] []
    | Some { s_display_type = Spice; s_port = None } ->
       e "graphics" [ "type", "spice"; "autoport", "yes" ] [] in

  (match source.s_display with
   | Some { s_keymap = Some km } -> append_attr ("keymap", km) graphics
   | Some { s_keymap = None } | None -> ());
  (match source.s_display with
   | Some { s_password = Some pw } -> append_attr ("passwd", pw) graphics
   | Some { s_password = None } | None -> ());
  (match source.s_display with
   | Some { s_listen = listen } ->
      (match listen with
       | LNoListen -> ()
       | LAddress a ->
          let sub = e "listen" [ "type", "address"; "address", a ] [] in
          append_child sub graphics
       | LNetwork n ->
          let sub = e "listen" [ "type", "network"; "network", n ] [] in
          append_child sub graphics
       | LSocket s ->
          let attrs = [ "type", "socket" ] @
            match s with None -> [] | Some s -> [ "socket", s ] in
          let sub = e "listen" attrs [] in
          append_child sub graphics
       | LNone ->
          let sub = e "listen" [ "type", "none" ] [] in
          append_child sub graphics
      )
   | None -> ());
  List.push_back devices graphics;

  let sound =
    match source.s_sound with
    | None -> []
    | Some { s_sound_model = model } ->
       if qemu_supports_sound_card model then
         [ e "sound" [ "model", string_of_source_sound_model model ] [] ]
       else
         [] in
  List.push_back_list devices sound;

  (* Miscellaneous KVM devices. *)
  if guestcaps.gcaps_virtio_rng then
    List.push_back devices (
      e "rng" ["model", virtio_model] [
        (* XXX Using /dev/urandom requires libvirt >= 1.3.4.  Libvirt
         * was broken before that.
         *)
        e "backend" ["model", "random"] [PCData "/dev/urandom"]
      ]
    );
  (* For the balloon device, libvirt adds an implicit device
   * unless we use model='none', hence this:
   *)
  List.push_back devices (
    e "memballoon"
      ["model",
       if guestcaps.gcaps_virtio_balloon then virtio_model else "none"]
      []
  );
  if guestcaps.gcaps_isa_pvpanic then
    List.push_back devices (
      e "panic" ["model", "isa"] [
        e "address" ["type", "isa"; "iobase", "0x505"] []
      ]
    );
  if guestcaps.gcaps_virtio_socket then
    List.push_back devices (e "vsock" ["model", virtio_model] []);

  (* Standard devices added to every guest. *)
  List.push_back_list devices [
    e "input" ["type", "tablet"; "bus", "usb"] [];
    e "input" ["type", "mouse"; "bus", "ps2"] [];
    e "console" ["type", "pty"] [];
  ];

  (* Given that we install the QEMU Guest Agent for both Linux and Windows
   * guests unconditionally, create the virtio-serial device that's needed for
   * communication between the host and the agent.
   *)
  List.push_back_list devices [
    e "controller" ["type", "virtio-serial"; "model", virtio_model] [];
    e "channel" ["type", "unix"] [
      e "target" ["type", "virtio"; "name", "org.qemu.guest_agent.0"] []
    ]
  ];

  List.push_back_list body [
    e "devices" [] !devices;
  ];

  let doc : doc =
    doc "domain" [
      "type", "kvm";                (* Always assume target is kvm? *)
    ] !body in

  doc
