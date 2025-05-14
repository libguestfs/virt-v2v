(* virt-v2v-open
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
open Unix_utils
open Common_gettext.Gettext
open Getopt.OptionName

open Types
open Utils

let template_rex = PCRE.compile "@@"

module G = Guestfs

let rec main () =
  let set_string_option_once optname optref arg =
    match !optref with
    | Some _ ->
       error (f_"%s option used more than once on the command line") optname
    | None ->
       optref := Some arg
  in

  let input_conn = ref None in
  let input_format = ref None in
  let input_password = ref None in
  let input_transport = ref None in
  let input_options = ref [] in
  let io_query = ref false in
  let set_input_option_compat k v =
    List.push_back input_options (k, v)
  in
  let set_input_option option =
    if option = "?" then io_query := true
    else (
      let k, v = String.split "=" option in
      set_input_option_compat k v
    )
  in

  let input_modes =
    Select_input.input_modes |>
    List.map Select_input.string_of_input_mode |>
    String.concat "|" in
  let input_mode = ref None in
  let set_input_mode mode =
    if !input_mode <> None then
      error (f_"%s option used more than once on the command line") "-i";
    input_mode := Some (Select_input.input_mode_of_string mode)
  in

  let command_template = ref None in

  let argspec = [
    [ S 'i' ],       Getopt.String (input_modes, set_input_mode),
                                    s_"Set input mode (default: libvirt)";
    [ M"ic" ],       Getopt.String ("uri", set_string_option_once "-ic" input_conn),
                                    s_"Libvirt URI";
    [ M"if" ],       Getopt.String ("format", set_string_option_once "-if" input_format),
                                    s_"Input format";
    [ M"io" ],       Getopt.String ("option[=value]", set_input_option),
                                    s_"Set option for input mode";
    [ M"ip" ],       Getopt.String ("filename", set_string_option_once "-ip" input_password),
                                    s_"Use password from file to connect to input hypervisor";
    [ M"it" ],       Getopt.String ("transport", set_string_option_once "-it" input_transport),
                                    s_"Input transport";
    [ L"run" ],      Getopt.String ("COMMAND", set_string_option_once "--run" command_template),
                                    s_"External command to run";
  ] in

  let args = ref [] in
  let anon_fun s = List.push_front s args in
  let usage_msg =
    sprintf (f_"\
%s: open the virt-v2v input and run a program on it

virt-v2v-open -i disk disk.img --run 'virt-inspector --format=raw @@'

A short summary of the options is given below.  For detailed help please
read the man page virt-v2v-open(1).
")
      prog in

  let opthandle = create_standard_options argspec ~anon_fun ~key_opts:false
                    ~machine_readable:true usage_msg in
  Getopt.parse opthandle.getopt;

  (* Print the version, easier than asking users to tell us. *)
  debug "info: %s: %s %s (%s)"
        prog Config.package_name Config.package_version_full
        Config.host_cpu;

  (* Print the libvirt version if debugging. *)
  if verbose () then (
    let major, minor, release = Libvirt_utils.libvirt_get_version () in
    debug "info: libvirt version: %d.%d.%d" major minor release
  );

  (* Create the v2v directory to control conversion. *)
  let v2vdir = create_v2v_directory () in

  (* Dereference the arguments. *)
  let args = List.rev !args in
  let input_conn = !input_conn in
  let input_mode = !input_mode in
  let input_transport =
    match !input_transport with
    | None -> None
    | Some "ssh" -> Some Input.SSH
    | Some "vddk" -> Some Input.VDDK
    | Some transport ->
       error (f_"unknown input transport ‘-it %s’") transport in

  let command_template =
    match !command_template with
    | None -> error (f_"you must supply the --run parameter")
    | Some c -> c in

  (* No arguments and machine-readable mode?  Print out some facts
   * about what this binary supports.
   *)
  (match args, machine_readable () with
   | [], Some { pr } ->
      pr "virt-v2v-open\n";
      pr "libguestfs-rewrite\n";
      pr "colours-option\n";
      pr "io\n";
      pr "input:disk\n";
      pr "input:libvirt\n";
      pr "input:libvirtxml\n";
      pr "input:ova\n";
      pr "input:vmx\n";
      exit 0
   | _, _ -> ()
  );

  (* Select the input module. *)
  let (module Input_module) =
    Select_input.select_input input_mode input_conn input_transport in

  let input_options = {
    Input.bandwidth = None;
    input_conn = input_conn;
    input_format = !input_format;
    input_options = !input_options;
    input_password = !input_password;
    input_transport = input_transport;
    (* This must always be true so that we do not modify the
     * source.  This is set to [false] by in-place mode.
     *)
    read_only = true;
  } in

  (* If -io ? then we want to query input options supported in this mode. *)
  if !io_query then (
    Input_module.query_input_options ();
    exit 0
  );

  (* Before starting the input module, check there is sufficient
   * free space in the temporary directory on the host.
   *)
  check_host_free_space ();

  (* Start the input module (runs an NBD server in the background). *)
  message (f_"Setting up the source: %s")
    (Input_module.to_string input_options args);
  let source, input_disks = Input_module.setup v2vdir input_options args in

  message (f_"Running external command");

  (* We're not really doing a conversion here, but write the 'convert'
   * file around this to tune the input module correctly.
   *)
  with_open_out (v2vdir // "convert") (fun _ -> ());

  (* Get the list of input sockets (NBD endpoints), formatted as
   * a shell-quoted list of [-a] options.
   *)
  let add_params =
    List.map (
      fun uri ->
        let uri = NBD_URI.to_uri uri in
        sprintf "-a %s" (quote uri)
    ) input_disks
    |> String.concat " " in

  (* Run external program. *)
  debug "command template: %S" command_template;
  let cmd = PCRE.replace template_rex add_params command_template in
  let r = shell_command cmd in
  if r <> 0 then exit r;

  unlink (v2vdir // "convert");

  (* Debug the v2vdir. *)
  if verbose () then (
    let cmd = sprintf "ls -alZ %s 1>&2" (quote v2vdir) in
    ignore (Sys.command cmd)
  );

  message (f_"Finishing off");
  (* As the last thing, write a file indicating success before
   * we exit (so before we kill the helpers).  The helpers may
   * use the presence or absence of the file to determine if
   * on-success or on-fail cleanup is required.
   *)
  with_open_out (v2vdir // "done") (fun _ -> ())

(* Some input modules use large_tmpdir to unpack OVAs or store qcow2
 * overlays and some output modules use it to store temporary files.
 * In addition the  500 MB guestfs appliance may be created there.
 * (RHBZ#1316479, RHBZ#2051394)
 *)
and check_host_free_space () =
  let free_space = StatVFS.free_space (StatVFS.statvfs large_tmpdir) in
  debug "check_host_free_space: large_tmpdir=%s free_space=%Ld"
        large_tmpdir free_space;
  if free_space < 1_073_741_824L then
    error (f_"insufficient free space in the conversion server \
              temporary directory %s (%s).\n\nEither free up space \
              in that directory, or set the LIBGUESTFS_CACHEDIR \
              environment variable to point to another directory \
              with more than 1GB of free space.\n\nSee also the \
              virt-v2v(1) manual, section \
              \"Minimum free space check in the host\".")
      large_tmpdir (human_size free_space)

let () = run_main_and_handle_errors main
