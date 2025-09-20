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

(** nbdkit as an abstract data type.

    This "standalone" library can be used to examine nbdkit on
    the system, probe for plugins and filters, and construct
    and run nbdkit commands. *)

val is_installed : unit -> bool
(** Return true iff nbdkit is installed and passes some
    rudimentary tests that it is working.  Note this may
    return true even if none of the basic plugins are
    installed. *)

(** {2 Probe configuration of nbdkit} *)

type config = (string * string) list

val config : unit -> config
(** Returns the list of tuples from the [nbdkit --dump-config] command. *)

type version = int * int * int
(** Version of nbdkit: [major, minor, release].  The major
    will always be 1. *)

val version : unit -> version
(** Get the installed version of nbdkit. *)

val probe_server_parameter : string -> bool
(** Probe if a particular server parameter is available.

    eg. [probe_server_parameter "--name"] tests if the [--name]
    parameter is available in this build of nbdkit. *)

val probe_plugin : string -> bool
(** Probe if a particular plugin is available. *)

val probe_plugin_parameter : string -> string -> bool
(** Probe if a particular plugin parameter is available.

    [probe_plugin_parameter filter regex] greps for regex
    in the output of [nbdkit plugin --help]. *)

val probe_filter : string -> bool
(** Probe if a particular filter is available. *)

val probe_filter_parameter : string -> string -> bool
(** Probe if a particular filter parameter is available.

    [probe_filter_parameter filter regex] greps for regex
    in the output of [nbdkit --filter=filter null --help]. *)

(** {2 Create an nbdkit command and run it} *)

type cmd
(** An nbdkit command. *)

val create : ?quiet:bool -> ?name:string -> string -> cmd
(** Create a new nbdkit command.

    The parameter is the required plugin name.

    Normally the nbdkit verbose ([-v]) flag is inherited from
    virt-v2v but exceptionally you can use [~quiet:true] to make
    nbdkit always quiet.

    The optional [?name] parameter can be used to name this
    nbdkit instance (the name appears in debugging messages
    if using nbdkit >= 1.46). *)

val add_debug_flag : cmd -> string -> string -> unit
(** Add a debug flag ([-D] option). *)

val set_readonly : cmd -> bool -> unit
(** If true, set the readonly flag ([-r] option). *)

val set_threads : cmd -> int -> unit
(** Set the number of threads ([--threads] option).
    The default is 16. *)

val add_filter : cmd -> string -> unit
(** Add a filter.  You may need to use {!probe_filter} first to check
    the filter is installed, otherwise nbdkit will fail to run.
    The filters are added closest to the plugin first. *)

val add_filter_if_available : cmd -> string -> unit
(** Same as {!add_filter} but does the {!probe_filter} check and
    omits the filter if it's not available. *)

val add_arg : cmd -> string -> string -> unit
val add_args : cmd -> (string * string) list -> unit
(** Add a key=value argument(s) to the command line.

    The arguments are added left to right. *)

val add_env : cmd -> string -> string -> unit
(** Add name=value environment variable. *)

val run_unix : string -> cmd -> string * int
(** Start nbdkit command listening on a Unix domain socket, waiting
    for the process to start up.

    Returns the Unix domain socket name and the nbdkit process ID.

    The [--exit-with-parent], [--foreground], [--pidfile], [--newstyle]
    and [--unix] flags are added automatically.  Other flags are set as
    in the {!cmd} struct. *)
