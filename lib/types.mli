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

(** Types.

    This module contains the data types used throughout virt-v2v.

    There is a progression during conversion: source -> overlay ->
    target: We start with a description of the source VM (or physical
    machine for virt-p2v) with one or more source disks.  We place
    wriable overlay(s) on top of the source disk(s).  We do the
    conversion into the overlay(s).  We copy the overlay(s) to the
    target disk(s).

    (This progression does not apply for in-place conversions
    which happen on the source only.)

    Overlay disks contain a pointer back to source disks.
    Target disks contain a pointer back to overlay disks.

{v
┌──────┐
│source│
│struct│
└──┬───┘
   │ source.s_disks
   │
   │    ┌───────┐  ┌───────┐  ┌───────┐
   └────┤ disk1 ├──┤ disk2 ├──┤ disk3 │  Source disks
        └───▲───┘  └───▲───┘  └───▲───┘
            │          │          │
            │          │          │ overlay.ov_source
        ┌───┴───┐  ┌───┴───┐  ┌───┴───┐
        │ ovl1  ├──┤ ovl2  ├──┤ ovl3  │  Overlay disks
        └───▲───┘  └───▲───┘  └───▲───┘
            │          │          │
            │          │          │ target.target_overlay
        ┌───┴───┐  ┌───┴───┐  ┌───┴───┐
        │ targ1 ├──┤ targ2 ├──┤ targ3 │  Target disks
        └───────┘  └───────┘  └───────┘
v}
*)

(** {2 Source, source disks} *)

type source = {
  s_hypervisor : source_hypervisor;     (** Source hypervisor. *)
  s_name : string;                      (** Guest name. *)
  s_genid : string option;              (** VM Generation ID. *)
  s_memory : int64;                     (** Memory size (bytes). *)
  s_vcpu : int;                         (** Number of CPUs. *)
  s_cpu_vendor : string option;         (** Source CPU vendor. *)
  s_cpu_model : string option;          (** Source CPU model. *)
  s_cpu_topology : source_cpu_topology option; (** Source CPU topology. *)
  s_features : string list;             (** Machine features. *)
  s_firmware : source_firmware;         (** Firmware (BIOS or EFI). *)
  s_display : source_display option;    (** Guest display. *)
  s_sound : source_sound option;        (** Sound card. *)
  s_disks : source_disk list;           (** Source disks. *)
  s_removables : source_removable list; (** CDROMs etc. *)
  s_nics : source_nic list;             (** NICs. *)
}
(** The source: metadata, disk images. *)

and source_hypervisor =
  | QEmu | KQemu | KVM | Xen | LXC | UML | OpenVZ
  | Test | VMware | HyperV | VBox | Phyp | Parallels
  | Bhyve
  | Physical (** used by virt-p2v *)
  | UnknownHV (** used by -i disk *)
  | OtherHV of string
(** Possible source hypervisors.  See
    [libvirt.git/docs/schemas/domaincommon.rng] for the list supported
    by libvirt. *)

and source_firmware =
  | BIOS                                (** PC BIOS or default firmware *)
  | UEFI                                (** UEFI *)
  | UnknownFirmware                     (** Unknown: try to autodetect. *)
(** The firmware from the source metadata.  Note that
    [UnknownFirmware] state corresponds to disks (where we have no
    metadata) and temporarily also to libvirt because of
    RHBZ#1217444. *)

and source_disk = {
  s_disk_id : int;                      (** A unique ID for each source disk. *)
  s_controller : s_controller option;   (** Controller, eg. IDE, SCSI. *)
}
(** A source disk. *)

and s_controller = Source_IDE | Source_SATA | Source_SCSI | Source_NVME |
                   Source_virtio_blk | Source_virtio_SCSI
(** Source disk controller. *)

and source_removable = {
  s_removable_type : s_removable_type;  (** Type.  *)
  s_removable_controller : s_controller option; (** Controller, eg. IDE, SCSI.*)
  s_removable_slot : int option; (** Slot, eg. hda = 0, hdc = 2 *)
}
(** Removable media. *)

and s_removable_type = CDROM | Floppy

and source_nic = {
  s_mac : string option;                (** MAC address. *)
  s_nic_model : s_nic_model option;     (** Network adapter model. *)
  s_vnet : string;                      (** Source network name. *)
  s_vnet_type : vnet_type;              (** Source network type. *)
}
(** Network adapter models. *)
and s_nic_model = Source_other_nic of string |
                  Source_rtl8139 | Source_e1000 | Source_virtio_net
(** Network interfaces. *)
and vnet_type = Bridge | Network

and source_display = {
  s_display_type : s_display_type; (** Display type. *)
  s_keymap : string option;        (** Guest keymap. *)
  s_password : string option;      (** If required, password to access
                                       the display. *)
  s_listen : s_display_listen;     (** Listen address. *)
  s_port : int option;             (** Display port. *)
}
and s_display_type = Window | VNC | Spice
and s_display_listen =
  | LNoListen                      (** No parseable <listen/> element. *)
  | LAddress of string             (** Listen address. *)
  | LNetwork of string             (** Listen network. *)
  | LSocket of string option       (** Listen Unix domain socket. *)
  | LNone                          (** <listen type='none'> *)

and source_sound = {
  s_sound_model : source_sound_model; (** Sound model. *)
}
and source_sound_model =
  AC97 | ES1370 | ICH6 | ICH9 | PCSpeaker | SB16 | USBAudio

and source_cpu_topology = {
  s_cpu_sockets : int;             (** Number of sockets. *)
  s_cpu_cores : int;               (** Number of cores per socket. *)
  s_cpu_threads : int;             (** Number of threads per core. *)
}

val string_of_source : source -> string
val string_of_source_disk : source_disk -> string
val string_of_controller : s_controller -> string
val string_of_nic_model : s_nic_model -> string
val nic_model_of_string : string -> s_nic_model
val string_of_vnet_type : vnet_type -> string
val string_of_source_sound_model : source_sound_model -> string
val source_sound_model_of_string : string -> source_sound_model option
val string_of_source_cpu_topology : source_cpu_topology -> string

val string_of_source_hypervisor : source_hypervisor -> string
val source_hypervisor_of_string : string -> source_hypervisor

(** {2 Inspection data} *)

type inspect = {
  i_root : string;                      (** Root device. *)
  i_type : string;                      (** Usual inspection fields. *)
  i_distro : string;
  i_osinfo : string;
  i_arch : string;
  i_major_version : int;
  i_minor_version : int;
  i_package_format : string;
  i_package_management : string;
  i_product_name : string;
  i_product_variant : string;
  i_mountpoints : (string * string) list;
  i_apps : Guestfs.application2 list;   (** List of packages installed. *)
  i_apps_map : Guestfs.application2 list StringMap.t;
    (** This is a map from the app name to the application object.
        Since RPM allows multiple packages with the same name to be
        installed, the value is a list. *)
  i_firmware : i_firmware;
    (** The list of EFI system partitions for the guest with UEFI,
        otherwise the BIOS identifier. *)
  i_windows_systemroot : string;
  i_windows_software_hive : string;
  i_windows_system_hive : string;
  i_windows_current_control_set : string;
}
and i_firmware =
  | I_BIOS
  | I_UEFI of string list
(** Inspection information. *)

val string_of_inspect : inspect -> string

(** {2 Disk stats} *)
type disk_stats = {
  mutable target_actual_size : int64 option; (** Actual size on target. *)
}

(** {2 Overlay disks} *)

type overlay = {
  ov_overlay_file : string;  (** Local overlay file (qcow2 format). *)
  ov_sd : string;            (** "sda", "sdb" etc - canonical device name. *)
  ov_virtual_size : int64;   (** Virtual disk size in bytes. *)

  (* Note: The ov_source is for information ONLY (eg. printing
   * error messages).  It must NOT be opened/read/modified.
   *)
  ov_source : source_disk;   (** Link back to the source disk. *)

  ov_stats : disk_stats;     (** Size stats for this disk. *)
}
(** Overlay disk. *)

val string_of_overlay : overlay -> string

(** {2 Target disks} *)

type target = {
  target_file : target_file; (** Destination file or QEMU URI. *)
  target_format : string;    (** Destination format (eg. -of option). *)

  target_overlay : overlay;  (** Link back to the overlay disk. *)
}
(** Target disk. *)

and target_file =
  | TargetFile of string     (** Target is a file. *)
  | TargetURI of string      (** Target is a QEMU URI. *)

val string_of_target : target -> string

(** {2 Guest firmware} *)

type target_firmware = TargetBIOS | TargetUEFI

val string_of_target_firmware : target_firmware -> string

(** {2 Target NICs} *)

type target_nics = source_nic list

(** {2 Guest capabilities} *)

type guestcaps = {
  gcaps_block_bus : guestcaps_block_type;
  gcaps_net_bus : guestcaps_net_type;
  (** Best block device and network device guest can access.  These are
      determined during conversion by inspecting the guest (and in some cases
      conversion can actually enhance these by installing drivers).  Thus this
      is not known until after conversion. *)

  gcaps_virtio_rng : bool;      (** Guest supports virtio-rng. *)
  gcaps_virtio_balloon : bool;  (** Guest supports virtio balloon. *)
  gcaps_isa_pvpanic : bool;     (** Guest supports ISA pvpanic device. *)
  gcaps_virtio_socket : bool;   (** Guest supports virtio socket. *)
  gcaps_machine : guestcaps_machine; (** Machine model. *)
  gcaps_arch : string;          (** Architecture that KVM must emulate. *)
  gcaps_acpi : bool;            (** True if guest supports acpi. *)

  gcaps_virtio_1_0 : bool;
  (** The guest supports the virtio devices that it does at the virtio-1.0
      protocol level. *)
}
(** Guest capabilities after conversion.  eg. Was virtio found or installed? *)

and guestcaps_block_type = Virtio_blk | IDE
and guestcaps_net_type = Virtio_net | E1000 | RTL8139
and guestcaps_machine = I440FX | Q35 | Virt

val string_of_guestcaps : guestcaps -> string

(** {2 Guest buses} *)

type target_buses = {
  target_virtio_blk_bus : target_bus_slot array;
  target_ide_bus : target_bus_slot array;
  target_scsi_bus : target_bus_slot array;
  target_floppy_bus : target_bus_slot array;
}
(** Mapping of fixed and removable disks to buses.

    As shown in the diagram below, there are (currently) four buses
    attached to the target VM.  Each contains a chain of fixed or
    removable disks.  Slots can also be empty.

    We try to assign disks to the same slot number as they would
    occupy on the source, although that is not always possible.

{v
┌──────┐
│Target│
│  VM  │
└──┬───┘
   │    ┌─────┐   ┌─────┐   ┌─────┐
   ├────┤ sda ├───┤  -  ├───┤ sdc │  SCSI bus
   │    └─────┘   └─────┘   └─────┘
   │    ┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐
   ├────┤ hda ├───┤ hdb ├───┤ hdc ├───┤ hdd │  IDE bus
   │    └─────┘   └─────┘   └─────┘   └─────┘
   │    ┌─────┐   ┌─────┐
   ├────┤  -  ├───┤ vdb │  Virtio-blk bus
   │    └─────┘   └─────┘
   │    ┌─────┐
   └────┤ fda │  Floppy disks
        └─────┘
v}
 *)

and target_bus_slot =
| BusSlotEmpty                  (** This bus slot is empty. *)
| BusSlotDisk of source_disk    (** Contains a fixed disk. *)
| BusSlotRemovable of source_removable (** Contains a removable CD/floppy. *)

val string_of_target_buses : target_buses -> string

(** {2 Target metadata super-struct}

    This contains all target-related metadata in a single struct
    to simplify calling [output#create_metadata] *)

type target_meta = {
  guestcaps : guestcaps;
  target_buses : target_buses;
  target_firmware : target_firmware;
  target_nics : target_nics
}

(** {2 Command line parameters} *)

type root_choice = AskRoot | SingleRoot | FirstRoot | RootDev of string
(** Type of [--root] (root choice) option. *)

type output_allocation = Sparse | Preallocated
(** Type of [-oa] (output allocation) option. *)

type bandwidth =
| StaticBandwidth of string
| DynamicBandwidth of string option * string
(** [--bandwidth] and [--bandwidth-file] options. *)

type static_ip = {
  if_mac_addr : string;
  if_ip_address : string;
  if_default_gateway : string option;
  if_prefix_length : int option;
  if_nameservers : string list;
}
(** [--mac ..:ip:..] option. *)
