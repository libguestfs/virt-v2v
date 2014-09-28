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
open Unix

open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Types
open Utils

open Output

module QEMU = struct
  type poptions = bool * bool *
                  Types.output_allocation * string * string * string

  type t = unit

  let to_string options =
    "-o qemu" ^
      match options.output_storage with
      | Some os -> " -os " ^ os
      | None -> ""

  let query_output_options () =
    printf (f_"Output options (-oo) which can be used with -o qemu:

  -oo compressed      Compress the output file (used only with -of qcow2)
  -oo qemu-boot       Boot the guest in qemu after conversion
")

  let parse_options options source =
    if options.output_password <> None then
      error_option_cannot_be_used_in_output_mode "qemu" "-op";

    let compressed = ref false
    and qemu_boot = ref false in
    List.iter (
      function
      | "compressed", "" -> compressed := true
      | "compressed", v -> compressed := bool_of_string v
      | "qemu-boot", "" -> qemu_boot := true
      | "qemu-boot", v -> qemu_boot := bool_of_string v
      | k, _ ->
         error (f_"-o qemu: unknown output option ‘-oo %s’") k
    ) options.output_options;
    let compressed = !compressed
    and qemu_boot = !qemu_boot in

    (* -os must be set to a directory. *)
    let output_storage =
      match options.output_storage with
      | None ->
         error (f_"-o qemu: output directory was not specified, use '-os /dir'")
      | Some d when not (is_directory d) ->
         error (f_"-os %s: output directory does not exist or is \
                   not a directory") d
      | Some d -> d in

    let output_name = Option.value ~default:source.s_name options.output_name in

    (compressed, qemu_boot, options.output_alloc, options.output_format,
     output_name, output_storage)

  let setup dir options source input_disks =
    let input_sizes = get_disk_sizes input_disks in
    let compressed, _, output_alloc, output_format,
        output_name, output_storage = options in

    let uris =
      List.mapi (
        fun i size ->
          let socket = sprintf "%s/out%d" dir i in
          On_exit.unlink socket;

          (* Create the actual output disk. *)
          let outdisk = disk_path output_storage output_name i in
          output_to_local_file ~compressed output_alloc output_format
            outdisk size socket;

          NBD_URI.Unix (socket, None)
      ) input_sizes in

    (), uris

  let finalize dir options () output_disks source inspect target_meta =
    let _, qemu_boot, output_alloc, output_format,
        output_name, output_storage = options in

    let { guestcaps; target_buses;
          target_firmware; target_boot_device } = target_meta in

    (* Start the shell script.  Write it to a temporary file
     * which we rename at the end.
     *)
    let file = output_storage // output_name ^ ".sh" in
    let tmpfile = file ^ ".tmp" in
    On_exit.unlink tmpfile;

    let chan = open_out tmpfile in
    let fpf fs = fprintf chan fs in
    fpf "#!/bin/sh -\n";
    fpf "\n";
    fpf "set -e\n";
    fpf "#set -x\n";
    fpf "\n";

    (* Allow the user to override our choice of machine type. *)
    let () =
      let machine_str =
        match guestcaps.gcaps_machine with
        | I440FX -> "pc"
        | Q35 -> "q35"
        | Virt -> "virt" in
      fpf "machine=%s\n" machine_str;
      fpf "\n" in

    (* If the firmware is UEFI, locate the OVMF files. *)
    (match target_firmware with
     | TargetBIOS -> ()
     | TargetUEFI ->
        let prefix =
          match guestcaps.gcaps_arch with
          | "x86_64" ->
             fpf "uefi_dir=/usr/share/OVMF\n"; "OVMF"
          | "aarch64" ->
             fpf "uefi_dir=/usr/share/AAVMF\n"; "AAVMF"
          | arch ->
             error (f_"don’t know how to convert UEFI guests \
                       for architecture %s")
               arch in
        fpf "uefi_code=\"$( \
             find $uefi_dir -name '%s_CODE*.fd' -print -quit )\"\n"
          prefix;
        fpf "uefi_vars_template=\"$( \
             find $uefi_dir -name '%s_VARS.fd' -print -quit )\"\n"
          prefix;
        fpf "\n";
        fpf "# Make a copy of the UEFI variables template\n";
        fpf "uefi_vars=\"$(mktemp)\"\n";
        fpf "cp \"$uefi_vars_template\" \"$uefi_vars\"\n";
        fpf "\n";
        fpf "smm=off\n";
    );

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

    if not guestcaps.gcaps_rtc_utc then arg "-rtc" "base=localtime";

    arg_noquote "-machine" "$machine${smm:+,smm=$smm},accel=kvm:tcg";

    fpf "if [ \"$uefi_code\" != \"\" ]; then\n";
    fpf "    uefi_args=\"\\\n";
    fpf "    -global driver=cfi.pflash01,property=secure,value=$smm \\\n";
    fpf "    -drive if=pflash,format=raw,file=$uefi_code,readonly=on \\\n";
    fpf "    -drive if=pflash,format=raw,file=$uefi_vars \\\n";
    fpf "    \"\n";
    fpf "fi\n";
    fpf "\n";
    Qemuopts.raw cmd "$uefi_args";

    arg "-m" (Int64.to_string (source.s_memory /^ 1024L /^ 1024L));

    arg "-cpu" (Option.value ~default:"host" source.s_cpu_model);

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

    (* For IDE disks, IDE CD-ROMs, SCSI disks, SCSI CD-ROMs, and floppies, we
     * need host-bus adapters (HBAs) between these devices and the PCI(e) root
     * bus. Some machine types create these HBAs automatically (despite
     * "-no-user-config -nodefaults"), some don't...
     *)
    let disk_cdrom_filter =
      function
      | BusSlotDisk _
      | BusSlotRemovable { s_removable_type = CDROM } -> true
      | _ -> false
    and floppy_filter =
      function
      | BusSlotRemovable { s_removable_type = Floppy } -> true
      | _ -> false
    in
    let ide_ctrl_needed =
      Array.exists disk_cdrom_filter target_buses.target_ide_bus
    and scsi_ctrl_needed =
      Array.exists disk_cdrom_filter target_buses.target_scsi_bus
    and floppy_ctrl_needed =
      Array.exists floppy_filter target_buses.target_floppy_bus in

    if ide_ctrl_needed then (
      match guestcaps.gcaps_machine with
      | I440FX -> ()
        (* The PC machine has a built-in controller of type "piix3-ide"
         * providing buses "ide.0" and "ide.1", with each bus fitting two
         * devices.
         *)
      | Q35 -> ()
        (* The Q35 machine has a built-in controller of type "ich9-ahci"
         * providing buses "ide.0" through "ide.5", with each bus fitting one
         * device.
         *)
      | Virt -> warning (f_"The Virt machine has no support for IDE. Please \
                            report a bug for virt-v2v -- refer to virt-v2v(1) \
                            section \"BUGS\".")
    );

    if scsi_ctrl_needed then
      (* We need to add the virtio-scsi HBA on all three machine types. The bus
       * provided by this device will be called "scsi0.0".
       *)
      arg_list "-device" [ "virtio-scsi-pci"; "id=scsi0" ];

    if floppy_ctrl_needed then (
      match guestcaps.gcaps_machine with
      | I440FX -> ()
        (* The PC machine has a built-in controller of type "isa-fdc"
         * providing bus "floppy-bus.0", fitting two devices.
         *)
      | Q35 -> arg_list "-device" [ "isa-fdc"; "id=floppy-bus" ]
        (* On the Q35 machine, we need to add the same HBA manually. Note that
         * the bus name will have ".0" appended automatically.
         *)
      | Virt -> warning (f_"The Virt machine has no support for floppies. \
                            Please report a bug for virt-v2v -- refer to \
                            virt-v2v(1) section \"BUGS\".")
    );

    let add_disk_backend disk_id backend_name =
      (* Add a drive (back-end) for a "virtio-blk-pci", "ide-hd", or "scsi-hd"
       * device (front-end). The drive has a backing file, identified by
       * "disk_id".
       *)
      let outdisk = disk_path output_storage output_name disk_id in
      let bootindex =
         match target_boot_device with
         | None -> disk_id+1
         | Some disk_index when disk_index = disk_id -> 1
         | Some _ -> disk_id+2 in
      arg_list "-drive" [ "file=" ^ outdisk;
                          "format=" ^ output_format;
                          "if=none";
                          "id=" ^ backend_name;
                          "media=disk";
                          sprintf "bootindex=%d" bootindex ]

    and add_cdrom_backend backend_name =
      (* Add a drive (back-end) for an "ide-cd" or "scsi-cd" device (front-end).
       * The drive is empty -- there is no backing file.
       *)
      arg_list "-drive" [ "if=none"; "id=" ^ backend_name; "media=cdrom" ]

    and add_floppy_backend backend_name =
      (* Add a drive (back-end) for a "floppy" device (front-end). The drive is
       * empty -- there is no backing file. *)
      arg_list "-drive" [ "if=none"; "id=" ^ backend_name; "media=disk" ]
    in

    let add_virtio_blk disk_id frontend_ctr =
      (* Create a "virtio-blk-pci" device (front-end), together with its drive
       * (back-end). The disk identifier is mandatory.
       *)
      let backend_name = sprintf "drive-vblk-%d" frontend_ctr in
      add_disk_backend disk_id backend_name;
      arg_list "-device" [ "virtio-blk-pci"; "drive=" ^ backend_name ]

    and add_ide disk_id frontend_ctr =
      (* Create an "ide-hd" or "ide-cd" device (front-end), together with its
       * drive (back-end). If a disk identifier is passed in, then "ide-hd" is
       * created (with a non-empty drive); otherwise, "ide-cd" is created (with
       * an empty drive).
       *)
      let backend_name = sprintf "drive-ide-%d" frontend_ctr
      and ide_bus, ide_unit =
        match guestcaps.gcaps_machine with
        | I440FX -> frontend_ctr / 2, frontend_ctr mod 2
        | Q35 -> frontend_ctr, 0
        | Virt -> 0, 0 (* should never happen, see warning above *) in
      let common_props = [ sprintf "bus=ide.%d" ide_bus;
                           sprintf "unit=%d" ide_unit;
                           "drive=" ^ backend_name ] in
      (match disk_id with
       | Some id ->
           add_disk_backend id backend_name;
           arg_list "-device" ("ide-hd" :: common_props)
       | None ->
           add_cdrom_backend backend_name;
           arg_list "-device" ("ide-cd" :: common_props))

    and add_scsi disk_id frontend_ctr =
      (* Create a "scsi-hd" or "scsi-cd" device (front-end), together with its
       * drive (back-end). If a disk identifier is passed in, then "scsi-hd" is
       * created (with a non-empty drive); otherwise, "scsi-cd" is created (with
       * an empty drive).
       *)
      let backend_name = sprintf "drive-scsi-%d" frontend_ctr in
      let common_props = [ "bus=scsi0.0";
                           sprintf "lun=%d" frontend_ctr;
                           "drive=" ^ backend_name ] in
      (match disk_id with
       | Some id ->
           add_disk_backend id backend_name;
           arg_list "-device" ("scsi-hd" :: common_props)
       | None ->
           add_cdrom_backend backend_name;
           arg_list "-device" ("scsi-cd" :: common_props))

    and add_floppy frontend_ctr =
      (* Create a "floppy" (front-end), together with its empty drive
       * (back-end).
       *)
      let backend_name = sprintf "drive-floppy-%d" frontend_ctr in
      add_floppy_backend backend_name;
      arg_list "-device" [ "floppy"; "bus=floppy-bus.0";
                           sprintf "unit=%d" frontend_ctr;
                           "drive=" ^ backend_name ]
    in

    (* Add virtio-blk-pci devices for BusSlotDisk elements on
     * "target_virtio_blk_bus".
     *)
    Array.iteri
      (fun frontend_ctr disk ->
         match disk with
         | BusSlotDisk d -> add_virtio_blk d.s_disk_id frontend_ctr
         | _ -> ())
      target_buses.target_virtio_blk_bus;

    let add_disk_or_cdrom bus_adder frontend_ctr slot =
      (* Add a disk or CD-ROM front-end to the IDE or SCSI bus. *)
      match slot with
      | BusSlotDisk d ->
          bus_adder (Some d.s_disk_id) frontend_ctr
      | BusSlotRemovable { s_removable_type = CDROM } ->
          bus_adder None frontend_ctr
      | _ -> ()
    in

    (* Add disks and CD-ROMs to the IDE and SCSI buses. *)
    Array.iteri (add_disk_or_cdrom add_ide) target_buses.target_ide_bus;
    Array.iteri (add_disk_or_cdrom add_scsi) target_buses.target_scsi_bus;

    (* Add floppies. *)
    Array.iteri
      (fun frontend_ctr disk ->
         match disk with
         | BusSlotRemovable { s_removable_type = Floppy } ->
             add_floppy frontend_ctr
         | _ -> ())
      target_buses.target_floppy_bus;

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
      arg "-device" "virtio-balloon";
    if guestcaps.gcaps_isa_pvpanic then
      arg_list "-device" ["pvpanic"; "ioport=0x505"];
    if guestcaps.gcaps_virtio_socket then (
      (* qemu requires a free guest CID to be chosen.  If you use libvirt
       * then it does this by iterating over the CIDs doing
       * ioctl(fd, VHOST_VSOCK_SET_GUEST_CID, &val) on each one until
       * it finds a free CID.  See:
       * https://bugzilla.redhat.com/show_bug.cgi?id=1291851#c6
       *
       * As that is essentially impossible to do from the shell script,
       * instead assign a semi-random one here.  Using the PID of
       * virt-v2v means that we're most likely to assign an unused
       * CID, especially with modern Linux which has a very large
       * PID space.  Note that CID must be [3..UINT32_MAX-1] and
       * max PID in Linux is 2^22.
       *)
      let pid = getpid () in
      let pid = max 3 pid in
      (* In OCaml 4.13 we could use this, if we were worried about a
         future system having very large PIDs:
         let pid = Int64.( min (max 3L (of_int (getpid ())))
                               (of_int32 Int32.max_int) ) in
       *)
      let guest_cid = sprintf "guest-cid=%d" pid in
      arg_list "-device" ["vhost-vsock-pci"; guest_cid ]
    );

    (* Add a serial console to Linux guests. *)
    if inspect.i_type = "linux" then
      arg "-serial" "stdio";

    (* Write the qemu command. *)
    Qemuopts.to_chan cmd chan;

    (* Finish off by renaming the temporary file to the final file
     * and making it executable.
     *)
    Unix.rename tmpfile file;
    Unix.chmod file 0o755;

    (* If -oo qemu-boot option was specified then we should boot the guest. *)
    if qemu_boot then (
      let cmd = sprintf "%s &" (quote file) in
      ignore (shell_command cmd)
    )

  let request_size = None
end
