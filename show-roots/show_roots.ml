(* virt-v2v-show-roots
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

module G = Guestfs

let rec main () =
  let set_string_option_once optname optref arg =
    match !optref with
    | Some _ ->
       error (f_"%s option used more than once on the command line") optname
    | None ->
       optref := Some arg
  in

  let output_file = ref None in
  let set_output_file_option filename =
    if filename = "-" then output_file := None
    else output_file := Some filename
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

  let input_mode = ref `Not_set in
  let set_input_mode mode =
    if !input_mode <> `Not_set then
      error (f_"%s option used more than once on the command line") "-i";
    match mode with
    | "disk" | "local" -> input_mode := `Disk
    | "libvirt" -> input_mode := `Libvirt
    | "libvirtxml" -> input_mode := `LibvirtXML
    | "ova" -> input_mode := `OVA
    | "vmx" -> input_mode := `VMX
    | s ->
       error (f_"unknown -i option: %s") s
  in

  let echo_keys = ref false in
  let set_echo_keys () = echo_keys := true in
  let keys_from_stdin = ref false in
  let set_keys_from_stdin () = keys_from_stdin := true in
  let key_options = ref [] in
  let add_key_option s = List.push_back key_options s in

  let argspec = [
    [ S 'i' ],       Getopt.String ("disk|libvirt|libvirtxml|ova|vmx", set_input_mode),
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
    [ S 'O' ],       Getopt.String ("output.xml", set_output_file_option),
                                    s_"Set the output filename";
    [ L"echo-keys" ],Getopt.Unit set_echo_keys,
                                    s_"Don’t turn off echo for passphrases";
    [ L"keys-from-stdin" ], Getopt.Unit set_keys_from_stdin,
                                    s_"Read passphrases from stdin";
    [ L"key" ],      Getopt.String (s_"SELECTOR", add_key_option),
                                    s_"Specify a LUKS key";
  ] in

  let args = ref [] in
  let anon_fun s = List.push_front s args in
  let usage_msg =
    sprintf (f_"\
%s: estimate disk space needed before virt-v2v conversion

virt-v2v-inspector -i disk disk.img [-O output.xml]

A short summary of the options is given below.  For detailed help please
read the man page virt-v2v-inspector(1).
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
  let output_file = !output_file in
  let input_conn = !input_conn in
  let input_mode = !input_mode in
  let input_transport =
    match !input_transport with
    | None -> None
    | Some "ssh" -> Some `SSH
    | Some "vddk" -> Some `VDDK
    | Some transport ->
       error (f_"unknown input transport ‘-it %s’") transport in
  let echo_keys = !echo_keys in
  let keys_from_stdin = !keys_from_stdin in
  let key_options = !key_options in

  (* No arguments and machine-readable mode?  Print out some facts
   * about what this binary supports.
   *)
  (match args, machine_readable () with
   | [], Some { pr } ->
      pr "virt-v2v-show-roots\n";
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

  (* Get the input module. *)
  let (module Input_module) =
    match input_mode with
    | `Disk -> (module Input_disk.Disk : Input.INPUT)
    | `LibvirtXML -> (module Input_libvirt.LibvirtXML)
    | `OVA -> (module Input_ova.OVA)
    | `VMX -> (module Input_vmx.VMX)
    | `Not_set | `Libvirt ->
       match input_conn with
       | None -> (module Input_libvirt.Libvirt_)
       | Some orig_uri ->
          let { Xml.uri_server = server; uri_scheme = scheme } =
            try Xml.parse_uri orig_uri
            with Invalid_argument msg ->
              error (f_"could not parse '-ic %s'.  \
                        Original error message was: %s")
                orig_uri msg in

          match server, scheme, input_transport with
          | None, _, _
            | Some "", _, _       (* Not a remote URI. *)

            | Some _, None, _     (* No scheme? *)
            | Some _, Some "", _ ->
             (module Input_libvirt.Libvirt_)

          (* vCenter over https. *)
          | Some server, Some ("esx"|"gsx"|"vpx"), None ->
             (module Input_vcenter_https.VCenterHTTPS)

          (* vCenter or ESXi using nbdkit vddk plugin *)
          | Some server, Some ("esx"|"gsx"|"vpx"), Some `VDDK ->
             (module Input_vddk.VDDK)

          (* Xen over SSH *)
          | Some server, Some "xen+ssh", _ ->
             (module Input_xen_ssh.XenSSH)

          (* Old virt-v2v also supported qemu+ssh://.  However I am
           * deliberately not supporting this in new virt-v2v.  Don't
           * use virt-v2v if a guest already runs on KVM.
           *)

          (* Unknown remote scheme. *)
          | Some _, Some _, _ ->
             warning (f_"no support for remote libvirt connections \
                         to '-ic %s'.  The conversion may fail when it \
                         tries to read the source disks.") orig_uri;
             (module Input_libvirt.Libvirt_) in

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
  let source = Input_module.setup v2vdir input_options args in

  message (f_"Running virt-inspector");

  (* We're not really doing a conversion here, but write the 'convert'
   * file around this to tune the input module correctly.
   *)
  with_open_out (v2vdir // "convert") (fun _ -> ());

  (* Get the list of input sockets (NBD endpoints). *)
  let add_params =
    List.map (
      fun { s_disk_id = i } ->
        let socket = sprintf "nbd+unix://?socket=%s/in%d" v2vdir i in
        sprintf "-a %s" (quote socket)
    ) source.s_disks
    |> String.concat " " in

  (* Get the list of --key and related options and pass them through. *)
  let key_params =
    List.map (fun opt -> sprintf "--key %s" (quote opt)) key_options
    |> String.concat " " in

  (* Run virt-inspector. *)
  let cmd = sprintf "virt-inspector --format=raw %s %s %s %s"
              add_params key_params
              (if echo_keys then "--echo-keys" else "")
              (if keys_from_stdin then "--keys-from-stdin" else "") in
  let xml_lines = external_command cmd in

  unlink (v2vdir // "convert");

  (* Debug the v2vdir. *)
  if verbose () then (
    let cmd = sprintf "ls -alZ %s 1>&2" (quote v2vdir) in
    ignore (Sys.command cmd)
  );

  (* Dump out the available information. *)
  let chan =
    match output_file with
    | None -> Stdlib.stdout
    | Some filename -> open_out filename in
  List.iter (fprintf chan "%s\n") xml_lines;
  Stdlib.flush chan;

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
