(* virt-v2v
 * Copyright (C) 2009-2021 Red Hat Inc.
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
open Unix

open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Types
open Utils

open Output

module QEMU = struct
  type poptions = bool * Types.output_allocation * string * string * string

  type t = unit

  let to_string options =
    "-o qemu" ^
      match options.output_storage with
      | Some os -> " -os " ^ os
      | None -> ""

  let query_output_options () =
    printf (f_"Output options (-oo) which can be used with -o qemu:

  -oo qemu-boot       Boot the guest in qemu after conversion
")

  let parse_options options source =
    if options.output_password <> None then
      error_option_cannot_be_used_in_output_mode "qemu" "-op";

    let qemu_boot = ref false in
    List.iter (
      fun (k, v) ->
        match k with
        | "qemu-boot" ->
           if v = "" || v = "true" then qemu_boot := true
           else if v = "false" then qemu_boot := false
           else
             error (f_"-o qemu: use -oo qemu-boot[=true|false]")
        | k ->
           error (f_"-o qemu: unknown output option ‘-oo %s’") k
      ) options.output_options;
    let qemu_boot = !qemu_boot in

    if qemu_boot then
      error (f_"-o qemu: the -oo qemu-boot option cannot be used in RHEL");

    (* -os must be set to a directory. *)
    let output_storage =
      match options.output_storage with
      | None ->
         error (f_"-o qemu: output directory was not specified, use '-os /dir'")
      | Some d when not (is_directory d) ->
         error (f_"-os %s: output directory does not exist or is not a directory") d
      | Some d -> d in

    let output_name = Option.default source.s_name options.output_name in

    (qemu_boot, options.output_alloc, options.output_format,
     output_name, output_storage)

  let setup dir options source =
    let disks = get_disks dir in
    let _, output_alloc, output_format, output_name, output_storage = options in

    List.iter (
      fun (i, size) ->
        let socket = sprintf "%s/out%d" dir i in
        On_exit.unlink socket;

        (* Create the actual output disk. *)
        let outdisk = disk_path output_storage output_name i in
        output_to_local_file output_alloc output_format outdisk size socket
    ) disks

  let finalize dir options () source inspect target_meta =
    let qemu_boot, output_alloc, output_format,
        output_name, output_storage = options in

    let { guestcaps; target_buses; target_firmware } = target_meta in

    (* Convert metadata to libvirt XML. *)
    (match target_firmware with
     | TargetBIOS -> ()
     | TargetUEFI ->
        (* XXX Can remove this method when libvirt supports
         * <loader type="efi"/> since then it will be up to
         * libvirt to check this.
         *)
        error_unless_uefi_firmware guestcaps.gcaps_arch
    );

    let file = output_storage // output_name ^ ".sh" in

    let uefi_firmware =
      match target_firmware with
      | TargetBIOS -> None
      | TargetUEFI -> Some (find_uefi_firmware guestcaps.gcaps_arch) in
    let machine, secure_boot_required =
      match guestcaps.gcaps_machine, uefi_firmware with
      | _, Some { Uefi.flags }
           when List.mem Uefi.UEFI_FLAG_SECURE_BOOT_REQUIRED flags ->
         (* Force machine type to Q35 because PC does not support
          * secure boot.  We must remove this when we get the
          * correct machine type from libosinfo in future. XXX
          *)
         Q35, true
      | machine, _ ->
         machine, false in
    let smm = secure_boot_required in

    let machine =
      match machine with
      | I440FX -> "pc"
      | Q35 -> "q35"
      | Virt -> "virt" in

    (* Construct the command line.  Note that the [Qemuopts]
     * module deals with shell and qemu comma quoting.
     *)
    let cmd = Qemuopts.create () in
    Qemuopts.set_binary cmd "/usr/libexec/qemu-kvm";

    let flag = Qemuopts.flag cmd
    and arg = Qemuopts.arg cmd
    and arg_noquote = Qemuopts.arg_noquote cmd
    and arg_list = Qemuopts.arg_list cmd in

    flag "-no-user-config"; flag "-nodefaults";
    arg "-name" output_name;

    (match source.s_genid with
     | None -> ()
     | Some genid ->
        arg_list "-device" ["vmgenid"; sprintf "guid=%s" genid; "id=vmgenid0"]
    );

    arg_list "-machine" (machine ::
                           (if smm then ["smm=on"] else []) @
                           ["accel=kvm:tcg"]);

    (match uefi_firmware with
     | None -> ()
     | Some { Uefi.code } ->
        if secure_boot_required then
          arg_list "-global"
            ["driver=cfi.pflash01"; "property=secure"; "value=on"];
        arg_list "-drive"
          ["if=pflash"; "format=raw"; "file=" ^ code; "readonly"];
        arg_noquote "-drive" "if=pflash,format=raw,file=\"$uefi_vars\"";
    );

    arg "-m" (Int64.to_string (source.s_memory /^ 1024L /^ 1024L));
    if source.s_vcpu > 1 then (
      (match source.s_cpu_topology with
       | None ->
          arg "-smp" (string_of_int source.s_vcpu)
       | Some { s_cpu_sockets; s_cpu_cores; s_cpu_threads } ->
          let args = [
              sprintf "cpus=%d" source.s_vcpu;
              sprintf "sockets=%d" s_cpu_sockets;
              sprintf "cores=%d" s_cpu_cores;
              sprintf "threads=%d" s_cpu_threads;
            ] in
          arg_list "-smp" args
      );
    );

    let make_disk if_name i = function
      | BusSlotEmpty -> ()

      | BusSlotDisk d ->
         (* Find the corresponding target disk. *)
         let outdisk = disk_path output_storage output_name d.s_disk_id in

         arg_list "-drive" ["file=" ^ outdisk; "format=" ^ output_format;
                            "if=" ^ if_name; "index=" ^ string_of_int i;
                            "media=disk"]

      | BusSlotRemovable { s_removable_type = CDROM } ->
         arg_list "-drive" ["format=raw"; "if=" ^ if_name;
                            "index=" ^ string_of_int i; "media=cdrom"]

      | BusSlotRemovable { s_removable_type = Floppy } ->
         arg_list "-drive" ["format=raw"; "if=" ^ if_name;
                            "index=" ^ string_of_int i; "media=floppy"]
    in
    Array.iteri (make_disk "virtio") target_buses.target_virtio_blk_bus;
    Array.iteri (make_disk "ide") target_buses.target_ide_bus;

    let make_scsi i = function
      | BusSlotEmpty -> ()

      | BusSlotDisk d ->
         (* Find the corresponding target disk. *)
         let outdisk = disk_path output_storage output_name d.s_disk_id in

         arg_list "-drive" ["file=" ^ outdisk; "format=" ^ output_format;
                            "if=scsi"; "bus=0"; "unit=" ^ string_of_int i;
                            "media=disk"]

      | BusSlotRemovable { s_removable_type = CDROM } ->
         arg_list "-drive" ["format=raw"; "if=scsi"; "bus=0";
                            "unit=" ^ string_of_int i; "media=cdrom"]

      | BusSlotRemovable { s_removable_type = Floppy } ->
         arg_list "-drive" ["format=raw"; "if=scsi"; "bus=0";
                            "unit=" ^ string_of_int i; "media=floppy"]
    in
    Array.iteri make_scsi target_buses.target_scsi_bus;

    (* XXX Highly unlikely that anyone cares, but the current
     * code ignores target_buses.target_floppy_bus.
     *)

    let net_bus =
      match guestcaps.gcaps_net_bus with
      | Virtio_net -> "virtio-net-pci"
      | E1000 -> "e1000"
      | RTL8139 -> "rtl8139" in
    List.iteri (
      fun i nic ->
        arg_list "-netdev" ["user"; "id=net" ^ string_of_int i];
        arg_list "-device" ([net_bus;
                             sprintf "netdev=net%d" i] @
                              (match nic.s_mac with
                               | None -> []
                               | Some mac -> ["mac=" ^ mac]))
      ) target_meta.target_nics;

    (* Add a display. *)
    (match source.s_display with
     | None -> ()
     | Some display ->
        (match display.s_display_type with
         | Window ->
            arg "-display" "gtk"
         | VNC ->
            arg "-display" "vnc=:0"
         | Spice ->
            arg_list "-spice" [sprintf "port=%d"
                                 (match display.s_port with
                                  | None -> 5900
                                  | Some p -> p);
                               "addr=127.0.0.1"]
        );
        arg "-vga" "std"
    );

    (* Add a sound card. *)
    (match source.s_sound with
     | None -> ()
     | Some { s_sound_model = model } ->
        if qemu_supports_sound_card model then (
          match model with
          | AC97      -> arg "-device" "AC97"
          | ES1370    -> arg "-device" "ES1370"
          | ICH6      -> arg "-device" "intel-hda"; arg "-device" "hda-duplex"
          (* XXX ich9 is a q35-only device, so it's not likely
             that this will work unless we can force q35 above: *)
          | ICH9      -> arg "-device" "ich9-intel-hda"
          | PCSpeaker -> arg "-soundhw" "pcspk" (* not qdev-ified *)
          | SB16      -> arg "-device" "sb16"
          | USBAudio  -> arg "-device" "usb-audio"
        )
    );

    (* Add the miscellaneous KVM devices. *)
    if guestcaps.gcaps_virtio_rng then (
      arg_list "-object" ["rng-random"; "filename=/dev/urandom"; "id=rng0"];
      arg_list "-device" ["virtio-rng-pci"; "rng=rng0"];
    );
    if guestcaps.gcaps_virtio_balloon then
      arg "-balloon" "virtio"
    else
      arg "-balloon" "none";
    if guestcaps.gcaps_isa_pvpanic then
      arg_list "-device" ["pvpanic"; "ioport=0x505"];
    if guestcaps.gcaps_virtio_socket then
      arg "-device" "vhost-vsock-pci";

    (* Add a serial console to Linux guests. *)
    if inspect.i_type = "linux" then
      arg "-serial" "stdio";

    (* Write the qemu script. *)
    with_open_out file (
      fun chan ->
        let fpf fs = fprintf chan fs in
        fpf "#!/bin/sh -\n";
        fpf "\n";

        (match uefi_firmware with
         | None -> ()
         | Some { Uefi.vars = vars_template } ->
            fpf "# Make a copy of the UEFI variables template\n";
            fpf "uefi_vars=\"$(mktemp)\"\n";
            fpf "cp %s \"$uefi_vars\"\n" (quote vars_template);
            fpf "\n"
        );

        Qemuopts.to_chan cmd chan
    );

    Unix.chmod file 0o755;

    (* If -oo qemu-boot option was specified then we should boot the guest. *)
    if qemu_boot then (
      let cmd = sprintf "%s &" (quote file) in
      ignore (shell_command cmd)
    )

  let request_size = None
end
