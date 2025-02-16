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

type options = {
  block_driver : Types.guestcaps_block_type; (** [--block-driver] option *)
  keep_serial_console : bool;
  ks : Tools_utils.key_store;      (** [--key] option *)
  network_map : Networks.t;        (** [-b] and [-n] options *)
  root_choice : Types.root_choice; (** [--root] option *)
  static_ips : Types.static_ip list; (** [--mac :ip:] option *)
  customize_ops : Customize_cmdline.ops; (** virt-customize options *)
}

val convert : string -> options -> Types.source -> Types.inspect * Types.target_meta
(** [convert dir options source]

    Convert a guest to run on KVM. *)
