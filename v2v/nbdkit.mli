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

(** nbdkit as an abstract data type.

    This "standalone" library can be used to examine nbdkit on
    the system, probe for plugins and filters, and construct
    and run nbdkit commands. *)

val is_installed : unit -> bool
(** Return true iff nbdkit is installed and passes some
    rudimentary tests that it is working.  Note this may
    return true even if none of the basic plugins are
    installed. *)

type config = (string * string) list

val config : unit -> config
(** Returns the list of tuples from the [nbdkit --dump-config] command. *)

type version = int * int * int
(** Version of nbdkit: (major, minor, release).  The major
    will always be [1]. *)

val version : config -> version
(** Get the installed version of nbdkit. *)

val probe_plugin : string -> bool
(** Probe if a particular plugin is available. *)

val probe_filter : string -> bool
(** Probe if a particular filter is available. *)

type cmd
(** An nbdkit command line.  Note this type is immutable. *)

val new_cmd : cmd
(** Return an empty command line.  {!set_plugin} must be called. *)

val add_debug_flag : cmd -> string -> string -> cmd
val set_exportname : cmd -> string -> cmd
val set_readonly : cmd -> bool -> cmd
val set_selinux_label : cmd -> string option -> cmd
val set_verbose : cmd -> bool -> cmd
(** Set various command line flags. *)

val set_plugin : cmd -> string -> cmd
(** Set the plugin name.  Use {!probe_plugin} first to check the
    plugin is installed. *)

val add_filter : cmd -> string -> cmd
(** Add a filter.  Use {!probe_filter} first to check the filter
    is installed.  The filters are added closest to the plugin first. *)

val add_filter_if_available : cmd -> string -> cmd
(** Same as {!add_filter} but does the {!probe_filter} check and
    omits the filter if it's not available. *)

val add_arg : cmd -> string -> string -> cmd
(** Add a key=value argument to the command line.  The arguments are
    added left to right. *)

val add_env : cmd -> string -> string -> cmd
(** Add name=value environment variable. *)

val run_unix : cmd -> string * int
(** Start nbdkit command listening on a Unix domain socket, waiting
    for the process to start up.

    Returns the temporary Unix domain socket name and the nbdkit
    process ID.

    The --exit-with-parent, --foreground, --pidfile, --newstyle and
    --unix flags are added automatically.  Other flags are set as
    in the {!cmd} struct. *)
