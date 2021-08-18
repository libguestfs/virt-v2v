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

(* Handles the virt-v2v command line, running the helper steps at the
 * right time, and multiplexing command line parameters to the
 * helpers.
 *)

open Printf
open Unix

open Std_utils
open Tools_utils
open Unix_utils
open Common_gettext.Gettext
open Getopt.OptionName

open Utils

(* Create the temporary directory to control conversion.
 * (This directory is cleaned up in the cleanup() function).
 * Because it contains sockets, if we're running as root then
 * we must make it executable by world.
 *)
let tmpdir =
  let tmpdir = Mkdtemp.temp_dir "v2v." in
  let running_as_root = geteuid () = 0 in
  if running_as_root then chmod tmpdir 0o711;
  tmpdir

(* Ensure the input and output helpers get killed on exit. *)
let i_pid = ref 0 and o_pid = ref 0

let cleanup () =
  (* Wait a short time for the helpers to exit before deleting
   * the v2v directory, since the helpers cleanups may need
   * files in there.
   *)
  let rec loop timeout pid =
    if timeout = 0 then ()
    else if pid_finished pid then ()
    else (
      Unix.sleep 1;
      loop (timeout-1) pid
    )
  and pid_finished pid =
    try fst (waitpid [WNOHANG] pid) <> 0 with Unix_error _ -> false
  in

  if !i_pid <> 0 then (
    kill !i_pid Sys.sigterm;
    loop 30 !i_pid;
    i_pid := 0
  );
  if !o_pid <> 0 then (
    kill !o_pid Sys.sigterm;
    loop 30 !o_pid;
    o_pid := 0
  );

  let cmd = sprintf "rm -rf %s" (quote tmpdir) in
  ignore (Sys.command cmd)

let rec main () =
  at_exit cleanup;

  let set_string_option_once optname optref arg =
    match !optref with
    | Some _ ->
       error (f_"%s option used more than once on the command line") optname
    | None ->
       optref := Some arg
  in

  (* Input options passed to helper-v2v-input-*. *)
  let i_options = ref [] in
  let io_query = ref false in
  let add_i_option k v =
    if k = "-io" && v = "?" then io_query := true; (* XXX *)
    List.push_front (k, v) i_options
  in
  let set_input_option_compat k v = add_i_option "-io" (k ^ "=" ^ v) in

  (* Output options passed to helper-v2v-output-*. *)
  let o_options = ref [] in
  let oo_query = ref false in
  let add_o_option k v =
    if k = "-oo" && v = "?" then oo_query := true; (* XXX *)
    List.push_front (k, v) o_options
  in
  let set_output_option_compat k v = add_i_option "-oo" (k ^ "=" ^ v) in

  (* Conversion options passed to helper-v2v-convert. *)
  let conv_options = ref [] in
  let add_conv_option k v = List.push_front (k, v) conv_options in

  (* Other options that we handle here. *)
  let do_copy = ref true in
  let in_place = ref false in
  let print_source = ref false in

  let input_conn = ref None in
  let input_transport = ref None in
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

  let output_name = ref None in
  let output_mode = ref `Not_set in
  let set_output_mode mode =
    if !output_mode <> `Not_set then
      error (f_"%s option used more than once on the command line") "-o";
    match mode with
    | "glance" -> output_mode := `Glance
    | "libvirt" -> output_mode := `Libvirt
    | "disk" | "local" -> output_mode := `Disk
    | "json" -> output_mode := `JSON
    | "null" -> output_mode := `Null
    | "openstack" | "osp" | "rhosp" -> output_mode := `Openstack
    | "ovirt" | "rhv" | "rhev" -> output_mode := `RHV
    | "ovirt-upload" | "ovirt_upload" | "rhv-upload" | "rhv_upload" ->
       output_mode := `RHV_Upload
    | "qemu" -> output_mode := `QEmu
    | "vdsm" -> output_mode := `VDSM
    | s ->
       error (f_"unknown -o option: %s") s
  in

  let output_alloc = ref `Not_set in
  let set_output_alloc mode =
    if !output_alloc <> `Not_set then
      error (f_"%s option used more than once on the command line") "-oa";
    match mode with
    | "sparse" -> output_alloc := `Sparse
    | "preallocated" -> output_alloc := `Preallocated
    | s ->
       error (f_"unknown -oa option: %s") s
  in

  (* Options that are ignored for backwards compatibility. *)
  let no_trim_warning _ =
    warning (f_"the --no-trim option has been removed and now does nothing")
  in
  let vmtype_warning _ =
    warning (f_"the --vmtype option has been removed and now does nothing")
  in

  let argspec = [
    [ L"bandwidth" ], Getopt.String ("bps", add_i_option "--bandwidth"),
      s_"Set bandwidth to bits per sec";
    [ L"bandwidth-file" ], Getopt.String ("filename", add_i_option "--bandwidth-file"),
      s_"Set bandwidth dynamically from file";
    [ S 'b'; L"bridge" ], Getopt.String ("in:out", add_conv_option "-b"),
      s_"Map bridge ‘in’ to ‘out’";
    [ L"compressed" ], Getopt.Unit (fun () -> add_o_option "-oo" "compressed"),
      s_"Compress output file (-of qcow2 only)";
    [ S 'i' ],       Getopt.String ("disk|libvirt|libvirtxml|ova|vmx", set_input_mode),
      s_"Set input mode (default: libvirt)";
    [ M"ic" ],       Getopt.String ("uri", set_string_option_once "-ic" input_conn),
      s_"Libvirt URI";
    [ M"if" ],       Getopt.String ("format", add_i_option "-if"),
      s_"Input format (for -i disk)";
    [ M"io" ],       Getopt.String ("option[=value]", add_i_option "-io"),
      s_"Set option for input mode";
    [ M"ip" ],       Getopt.String ("filename", add_i_option "-ip"),
      s_"Use password from file to connect to input hypervisor";
    [ M"it" ],       Getopt.String ("transport", set_string_option_once "-it" input_transport),
      s_"Input transport";
    [ L"in-place" ], Getopt.Set in_place,
      s_"Only tune the guest in the input VM";
    [ L"mac" ],      Getopt.String ("mac:network|bridge|ip:out", add_conv_option "--mac"),
      s_"Map NIC to network or bridge or assign static IP";
    [ S 'n'; L"network" ], Getopt.String ("in:out", add_conv_option "-n"),
      s_"Map network ‘in’ to ‘out’";
    [ L"no-copy" ],  Getopt.Clear do_copy,
      s_"Just write the metadata";
    [ L"no-trim" ],  Getopt.String ("-", no_trim_warning),
      s_"Ignored for backwards compatibility";
    [ S 'o' ],       Getopt.String ("glance|json|libvirt|local|null|openstack|qemu|rhv|rhv-upload|vdsm", set_output_mode),
      s_"Set output mode (default: libvirt)";
    [ M"oa" ],       Getopt.String ("sparse|preallocated", set_output_alloc),
      s_"Set output allocation mode";
    [ M"oc" ],       Getopt.String ("uri", add_o_option "-oc"),
      s_"Output hypervisor connection";
    [ M"of" ],       Getopt.String ("raw|qcow2", add_o_option "-of"),
      s_"Set output format";
    [ M"on" ],       Getopt.String ("name", set_string_option_once "-on" output_name),
      s_"Rename guest when converting";
    [ M"oo" ],       Getopt.String ("option[=value]", add_o_option "-oo"),
      s_"Set option for output mode";
    [ M"op" ],       Getopt.String ("filename", add_o_option "-op"),
      s_"Use password from file to connect to output hypervisor";
    [ M"os" ],       Getopt.String ("storage", add_o_option "-os"),
      s_"Set output storage location";
    [ L"password-file" ], Getopt.String ("filename", add_i_option "-ip"),
      s_"Same as ‘-ip filename’";
    [ L"print-source" ], Getopt.Set print_source,
      s_"Print source and stop";
    [ L"qemu-boot" ], Getopt.Unit (fun () -> add_o_option "-oo" "qemu-boot"),
      s_"Boot in qemu (-o qemu only)";
    [ L"root" ],     Getopt.String ("ask|... ", add_conv_option "--root"),
      s_"How to choose root filesystem";
    [ L"vddk-config" ], Getopt.String ("filename", set_input_option_compat "vddk-config"),
      s_"Same as ‘-io vddk-config=filename’";
    [ L"vddk-cookie" ], Getopt.String ("cookie", set_input_option_compat "vddk-cookie"),
      s_"Same as ‘-io vddk-cookie=filename’";
    [ L"vddk-libdir" ], Getopt.String ("libdir", set_input_option_compat "vddk-libdir"),
      s_"Same as ‘-io vddk-libdir=libdir’";
    [ L"vddk-nfchostport" ], Getopt.String ("nfchostport", set_input_option_compat "vddk-nfchostport"),
      s_"Same as ‘-io vddk-nfchostport=nfchostport’";
    [ L"vddk-port" ], Getopt.String ("port", set_input_option_compat "vddk-port"),
      s_"Same as ‘-io vddk-port=port’";
    [ L"vddk-snapshot" ], Getopt.String ("snapshot-moref", set_input_option_compat "vddk-snapshot"),
      s_"Same as ‘-io vddk-snapshot=snapshot-moref’";
    [ L"vddk-thumbprint" ], Getopt.String ("thumbprint", set_input_option_compat "vddk-thumbprint"),
      s_"Same as ‘-io vddk-thumbprint=thumbprint’";
    [ L"vddk-transports" ], Getopt.String ("transports", set_input_option_compat "vddk-transports"),
      s_"Same as ‘-io vddk-transports=transports’";
    [ L"vdsm-compat" ], Getopt.String ("0.10|1.1", set_output_option_compat "vdsm-compat"),
      s_"Same as ‘-oo vdsm-compat=0.10|1.1’";
    [ L"vdsm-image-uuid" ], Getopt.String ("uuid", set_output_option_compat "vdsm-image-uuid"),
      s_"Same as ‘-oo vdsm-image-uuid=uuid’";
    [ L"vdsm-vol-uuid" ], Getopt.String ("uuid", set_output_option_compat "vdsm-vol-uuid"),
      s_"Same as ‘-oo vdsm-vol-uuid=uuid’";
    [ L"vdsm-vm-uuid" ], Getopt.String ("uuid", set_output_option_compat "vdsm-vm-uuid"),
      s_"Same as ‘-oo vdsm-vm-uuid=uuid’";
    [ L"vdsm-ovf-output" ], Getopt.String ("dir", set_output_option_compat "vdsm-ovf-output"),
      s_"Same as ‘-oo vdsm-ovf-output=dir’";
    [ L"vdsm-ovf-flavour" ], Getopt.String ("ovirt|rhvexp", set_output_option_compat "vdsm-ovf-flavour"),
      s_"Same as ‘-oo vdsm-ovf-flavour=flavour’";
    [ L"vmtype" ],   Getopt.String ("-", vmtype_warning),
      s_"Ignored for backwards compatibility";
  ] in
  let args = ref [] in
  let anon_fun s = List.push_front s args in
  let usage_msg =
    sprintf (f_"\
%s: convert a guest to use KVM

virt-v2v -ic vpx://vcenter.example.com/Datacenter/esxi -os imported esx_guest

virt-v2v -ic vpx://vcenter.example.com/Datacenter/esxi esx_guest \
         -o rhv -os rhv.nfs:/export_domain --network ovirtmgmt

virt-v2v -i libvirtxml guest-domain.xml -o local -os /var/tmp

virt-v2v -i disk disk.img -o local -os /var/tmp

virt-v2v -i disk disk.img -o glance

There is a companion front-end called \"virt-p2v\" which comes as an
ISO or CD image that can be booted on physical machines.

A short summary of the options is given below.  For detailed help please
read the man page virt-v2v(1).
")
      prog in
  let opthandle = create_standard_options argspec ~anon_fun ~key_opts:true ~machine_readable:true usage_msg in
  Getopt.parse opthandle.getopt;

  (* -on option has to be passed to both the output and conversion helpers. *)
  (match !output_name with
   | None -> ()
   | Some n -> add_o_option "-on" n; add_conv_option "-on" n
  );

  (* -oa preallocated has to be passed to the output helper.  But also
   * we handle it here with nbdcopy.
   *)
  (match !output_alloc with
   | `Not_set | `Sparse -> ()
   | `Preallocated -> add_o_option "-oa" "preallocated"
  );

  (* Dereference the arguments. *)
  let args = List.rev !args in
  let conv_options = List.rev !conv_options in
  let do_copy = !do_copy in
  let in_place = !in_place in
  let input_conn = !input_conn in
  let input_mode = !input_mode in
  let input_transport =
    match !input_transport with
    | None -> None
    | Some "ssh" -> Some `SSH
    | Some "vddk" -> Some `VDDK
    | Some transport ->
       error (f_"unknown input transport ‘-it %s’") transport in
  let i_options = List.rev !i_options in
  let output_alloc =
    match !output_alloc with
    | `Not_set | `Sparse -> Types.Sparse
    | `Preallocated -> Types.Preallocated in
  let output_mode = !output_mode in
  let o_options = List.rev !o_options in
  let print_source = !print_source in

  (* --in-place isn't implemented yet - TODO *)
  if in_place then error "XXX --in-place option is not implemented yet";

  (* No arguments and machine-readable mode?  Print out some facts
   * about what this binary supports.
   *)
  (match args, machine_readable () with
   | [], Some { pr } ->
      pr "virt-v2v\n";
      pr "virt-v2v-2.0\n";
      pr "libguestfs-rewrite\n";
      pr "vcenter-https\n";
      pr "xen-ssh\n";
      pr "vddk\n";
      pr "colours-option\n";
      pr "vdsm-compat-option\n";
      pr "in-place\n";
      pr "io/oo\n";
      pr "mac-option\n";
      pr "bandwidth-option\n";
      pr "mac-ip-option\n";
      pr "input:disk\n";
      pr "input:libvirt\n";
      pr "input:libvirtxml\n";
      pr "input:ova\n";
      pr "input:vmx\n";
      pr "output:glance\n";
      pr "output:json\n";
      pr "output:libvirt\n";
      pr "output:local\n";
      pr "output:null\n";
      pr "output:openstack\n";
      pr "output:qemu\n";
      pr "output:rhv\n";
      pr "output:rhv-upload\n";
      pr "output:vdsm\n";
      pr "convert:linux\n";
      pr "convert:windows\n";
      List.iter (pr "ovf:%s\n") Create_ovf.ovf_flavours;
      exit 0
   | _, _ -> ()
  );

  (* We pass through standard flags --colours, -q, -v, -x and the
   * v2v temporary directory to all helpers.
   *)
  let std_args = ref [] in
  List.push_back std_args tmpdir;
  if colours () then List.push_back std_args "--colours";
  if quiet () then List.push_back std_args "-q";
  if verbose () then List.push_back std_args "-v";
  if trace () then List.push_back std_args "-x";
  List.push_back std_args "--program-name=virt-v2v";
  let std_args = !std_args in

  (* Get the input helper name and command line. *)
  let i_helper, i_extra_args =
    match input_mode with
    | `Disk -> "helper-v2v-input", [ "-im"; "disk" ]
    | `LibvirtXML ->
       "helper-v2v-input", [ "-im"; "libvirtxml" ]
    | `OVA -> "helper-v2v-input", [ "-im"; "ova" ]
    | `VMX ->
       let i_extra_args =
         match input_transport with
         | None -> []
         | Some `SSH -> [ "-it"; "ssh" ]
         | Some `VDDK -> error (f_"only ‘-it ssh’ can be used here") in
       "helper-v2v-input", [ "-im"; "vmx" ] @ i_extra_args
    | `Not_set | `Libvirt ->
       match input_conn with
       | None -> "helper-v2v-input", [ "-im"; "libvirt" ]
       | Some orig_uri ->
          let { Xml.uri_server = server; uri_scheme = scheme } =
            try Xml.parse_uri orig_uri
            with Invalid_argument msg ->
              error (f_"could not parse '-ic %s'.  Original error message was: %s")
                orig_uri msg in

          match server, scheme, input_transport with
          | None, _, _
            | Some "", _, _       (* Not a remote URI. *)

            | Some _, None, _     (* No scheme? *)
            | Some _, Some "", _ ->
             "helper-v2v-input", [ "-im"; "libvirt"; "-ic"; orig_uri ]

          (* vCenter over https. *)
          | Some server, Some ("esx"|"gsx"|"vpx"), None ->
             "helper-v2v-input", [ "-im"; "vcenter-https"; "-ic"; orig_uri ]

          (* vCenter or ESXi using nbdkit vddk plugin *)
          | Some server, Some ("esx"|"gsx"|"vpx"), Some `VDDK ->
             "helper-v2v-input", [ "-im"; "vddk"; "-ic"; orig_uri ]

          (* Xen over SSH *)
          | Some server, Some "xen+ssh", _ ->
             "helper-v2v-input", [ "-im"; "xen-ssh"; "-ic"; orig_uri ]

          (* Old virt-v2v also supported qemu+ssh://.  However I am
           * deliberately not supporting this in new virt-v2v.  Don't
           * use virt-v2v if a guest already runs on KVM.
           *)

          (* Unknown remote scheme. *)
          | Some _, Some _, _ ->
             warning (f_"no support for remote libvirt connections to '-ic %s'.  The conversion may fail when it tries to read the source disks.") orig_uri;
             "helper-v2v-input", [ "-im"; "libvirt"; "-ic"; orig_uri ] in

  (* The purpose of this is simply to fail early on if the
   * input helper we have chosen does not exist.
   *)
  if shell_command (sprintf "%s --version >/dev/null 2>&1" i_helper) <> 0 then
    error (f_"input helper command (%s) does not exist or is not working")
      i_helper;

  let i_cmd =
    i_helper :: std_args @ i_extra_args @
      List.flatten (List.map (fun (k, v) -> [k; v]) i_options) @
        args in

  (* If -io ? then we want to query input options supported in this mode. *)
  if !io_query then (
    if run_command i_cmd <> 0 then
      (* We assume the command already printed an error. *)
      exit 1;
    exit 0
  );

  (* Get the output helper name and command line. *)
  let o_helper, o_extra_args =
    match output_mode with
    | `Not_set | `Libvirt ->
       "helper-v2v-output", [ "-om"; "libvirt" ]
    | `Disk ->
       "helper-v2v-output", [ "-om"; "disk" ]
    | `Null ->
       "helper-v2v-output", [ "-om"; "null" ]
    | `QEmu ->
       "helper-v2v-output", [ "-om"; "qemu" ]
    | `Glance ->
       "helper-v2v-output", [ "-om"; "glance" ]
    | `Openstack ->
       "helper-v2v-output", [ "-om"; "openstack" ]
    | `RHV_Upload ->
       "helper-v2v-output", [ "-om"; "rhv-upload"]
    | `RHV ->
       "helper-v2v-output", [ "-om"; "rhv" ]
    | `VDSM ->
       "helper-v2v-output", [ "-om"; "vdsm" ]
    | `JSON ->
       "helper-v2v-output", [ "-om"; "json" ] in

  (* The purpose of this is simply to fail early on if the
   * output helper we have chosen does not exist.  If --print-source
   * is used then we will never call the output helper so don't
   * fail in that case (it is used a lot in tests).
   *)
  if not print_source &&
       shell_command (sprintf "%s --version >/dev/null 2>&1" o_helper) <> 0 then
    error (f_"output helper command (%s) does not exist or is not working")
      o_helper;

  let o_setup_cmd, o_final_cmd =
    let o_options =
      List.flatten (List.map (fun (k, v) -> [k; v]) o_options) in
    o_helper :: "setup" :: std_args @ o_extra_args @ o_options,
    o_helper :: "final" :: std_args @ o_extra_args @ o_options in

  (* If -oo ? then we want to query output options supported in this mode. *)
  if !oo_query then (
    if run_command o_setup_cmd <> 0 then
      (* We assume the command already printed an error. *)
      exit 1;
    exit 0
  );

  (* XXX This is a hack for -o rhv and -o vdsm where we must remove
   * the serial console for Linux conversions.  We don't do this for
   * -o rhv-upload, even though we probably should, which would
   * indicate that actually RHV is fine if we don't do this.
   *)
  let conv_remove_serial_console =
    match output_mode with
    | `RHV | `VDSM -> ["--remove-serial-console"]
    | _ -> [] in

  (* Get the conversion options. *)
  let conv_helper = "helper-v2v-convert" in
  let conv_cmd =
    conv_helper :: std_args @
    conv_remove_serial_console @
    List.flatten (List.map (fun (k, v) -> [k; v]) conv_options) in

  (* Start the input helper (runs in the background). *)
  i_pid := start_helper "input" i_helper i_cmd (tmpdir // "in.pid");

  (* If --print-source then print the source metadata and exit. *)
  if print_source then (
    let source =
      with_open_in (tmpdir // "source") (
        fun chan ->
          let ver = input_value chan in
          assert (ver = Utils.metaversion);
          (input_value chan : Types.source)
      ) in
    printf (f_"Source guest information (--print-source option):\n");
    printf "\n";
    printf "%s\n" (Types.string_of_source source);
    exit 0
  );

  (* Start the output helper (runs in the background). *)
  o_pid := start_helper "output" o_helper o_setup_cmd (tmpdir // "out.pid");

  (* Debug the v2vdir. *)
  if verbose () then (
    let cmd = sprintf "ls -alZ %s 1>&2" (quote tmpdir) in
    ignore (Sys.command cmd)
  );

  (* Do the conversion. *)
  with_open_out (tmpdir // "convert") (fun _ -> ());
  if run_command conv_cmd <> 0 then
    (* We assume the command already printed an error. *)
    exit 1;
  unlink (tmpdir // "convert");

  (* Do the copy. *)
  if do_copy then (
    with_open_out (tmpdir // "copy") (fun _ -> ());

    (* Get the list of disks and corresponding sockets. *)
    let rec loop acc i =
      let input_socket = sprintf "%s/in%d" tmpdir i
      and output_socket = sprintf "%s/out%d" tmpdir i in
      if Sys.file_exists input_socket && Sys.file_exists output_socket then
        loop ((i, input_socket, output_socket) :: acc) (i+1)
      else
        List.rev acc
    in
    let disks = loop [] 0 in
    let nr_disks = List.length disks in

    (* Copy the disks. *)
    List.iter (
      fun (i, input_socket, output_socket) ->
        message (f_"Copying disk %d/%d") (i+1) nr_disks;

        let input_uri = nbd_uri_of_socket input_socket
        and output_uri = nbd_uri_of_socket output_socket in

        (* In verbose mode print some information about each
         * side of the pipeline.
         *)
        if verbose () then (
          nbdinfo ~content:true input_uri;
          nbdinfo ~content:false output_uri
        );

        (* At the moment, unconditionally set nbdcopy --request-size
         * to 4M (up from the default of 256K).  With nbdkit + vddk +
         * cow + cow-block-size=1M this is necessary because requests
         * must be larger than the cow filter block size to avoid
         * breaking up reads.  It probably doesn't affect other
         * modes, but in future consider setting this only for
         * specific input modes that adjust cow-block-size.
         *)
        let request_size = Some (4*1024*1024) in

        nbdcopy ?request_size output_alloc input_uri output_uri
    ) disks;

    unlink (tmpdir // "copy")
  );

  (* Do the finalization step. *)
  message (f_"Creating output metadata");
  debug "running output finalization: %s"
    (String.concat " " (List.map quote o_final_cmd));
  if run_command o_final_cmd <> 0 then
    (* We assume the command already printed an error. *)
    exit 1;

  message (f_"Finishing off");
  (* As the last thing, write a file indicating success before
   * we exit (so before we kill the helpers).  The helpers may
   * use the presence or absence of the file to determine if
   * on-success or on-fail cleanup is required.
   *)
  with_open_out (tmpdir // "done") (fun _ -> ())

and start_helper helper_type prog cmd pidfile =
  debug "running %s helper: %s"
    helper_type (String.concat " " (List.map quote cmd));
  let args = Array.of_list cmd in
  let pid = fork () in
  if pid = 0 then execvp prog args; (* Child process. *)

  (* Wait for the pidfile to appear, but also check if the
   * PID is still running in case it exited early during start-up.
   *)
  let rec loop timeout =
    if Sys.file_exists pidfile then true
    else if timeout = 0 then false
    else if pid_finished pid then false
    else (
      Unix.sleep 1;
      loop (timeout-1)
    )
  and pid_finished pid =
    try fst (waitpid [WNOHANG] pid) <> 0 with Unix_error _ -> false
  in

  (* Note that it can take a long time for the input helper
   * to start up if it has to connect to a remote VMware server.
   * Or (crucially) if the user has to enter a password.
   *)
  if not (loop 60) then
    (* We assume the command already printed an error. *)
    exit 1;

  pid

and nbdcopy ?request_size output_alloc input_uri output_uri =
  (* XXX It's possible that some output modes know whether
   * --target-is-zero which would be a useful optimization.
   *)
  let cmd = ref [] in
  List.push_back_list cmd [ "nbdcopy"; input_uri; output_uri ];
  List.push_back cmd "--flush";
  (match request_size with
   | None -> ()
   | Some size -> List.push_back cmd (sprintf "--request-size=%d" size)
  );
  (*List.push_back cmd "--verbose";*)
  if not (quiet ()) then List.push_back cmd "--progress";
  if output_alloc = Types.Preallocated then List.push_back cmd "--allocated";
  let cmd = !cmd in

  if run_command cmd <> 0 then
    error (f_"nbdcopy command failed, see earlier error messages")

(* Run nbdinfo on a URI and dump the information to stderr.
 * However don't fail if nbdinfo is not installed since
 * this is just used for debugging.
 *)
and nbdinfo ?(content = false) uri =
  let cmd =
    sprintf "nbdinfo%s %s"
      (if content then " --content" else " --no-content") (quote uri) in
  ignore (Sys.command cmd)

(* Convert a Unix domain socket path to an NBD URI. *)
and nbd_uri_of_socket = sprintf "nbd+unix:///?socket=%s"

let () = run_main_and_handle_errors main
