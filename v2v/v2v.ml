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

open Types
open Utils

(* Matches --mac command line parameters. *)
let mac_re = PCRE.compile "^([[:xdigit:]]{2}:[[:xdigit:]]{2}:[[:xdigit:]]{2}:[[:xdigit:]]{2}:[[:xdigit:]]{2}:[[:xdigit:]]{2}):(network|bridge|ip):(.*)$"
let mac_ip_re = PCRE.compile "^([[:xdigit:]]|:|\\.)+$"

let rec main () =
  let set_string_option_once optname optref arg =
    match !optref with
    | Some _ ->
       error (f_"%s option used more than once on the command line") optname
    | None ->
       optref := Some arg
  in

  let bandwidth = ref None in
  let bandwidth_file = ref None in
  let block_driver = ref None in
  let input_conn = ref None in
  let input_format = ref None in
  let input_password = ref None in
  let input_transport = ref None in

  let input_options = ref [] in
  let io_query = ref false in
  let set_input_option option =
    if option = "?" then io_query := true
    else (
      let k, v = String.split "=" option in
      List.push_back input_options (k, v)
    )
  in

  let output_options = ref [] in
  let oo_query = ref false in
  let set_output_option option =
    if option = "?" then oo_query := true
    else (
      let k, v = String.split "=" option in
      List.push_back output_options (k, v)
    )
  in

  let parallel = ref 1 in
  let network_map = Networks.create () in
  let static_ips = ref [] in
  let rec add_network str =
    match String.split ":" str with
    | "", "" ->
       error (f_"invalid -n/--network parameter")
    | out, "" | "", out ->
       Networks.add_default_network network_map out
    | in_, out ->
       Networks.add_network network_map in_ out
  and add_bridge str =
    match String.split ":" str with
    | "", "" ->
       error (f_"invalid -b/--bridge parameter")
    | out, "" | "", out ->
       Networks.add_default_bridge network_map out
    | in_, out ->
       Networks.add_bridge network_map in_ out
  and add_mac str =
    if not (PCRE.matches mac_re str) then
      error (f_"cannot parse --mac \"%s\" parameter") str;
    let mac = PCRE.sub 1 and out = PCRE.sub 3 in
    match PCRE.sub 2 with
    | "network" ->
       Networks.add_mac network_map mac Network out
    | "bridge" ->
       Networks.add_mac network_map mac Bridge out
    | "ip" ->
       (match String.nsplit "," out with
        | [] -> error (f_"invalid --mac ip option")
        | [ip] -> add_static_ip mac ip "" "" []
        | [ip; gw] -> add_static_ip mac ip gw "" []
        | ip :: gw :: len :: nameservers ->
           add_static_ip mac ip gw len nameservers
       )
    | _ -> assert false
  and add_static_ip if_mac_addr if_ip_address if_default_gateway
                    if_prefix_length_str if_nameservers =
    (* Check the IP addresses and prefix length are sensible.  This
     * is only a very simple test that they are sane, since IP addresses
     * come in too many valid forms to check thoroughly.
     *)
    let rec error_unless_ip_addr what addr =
      if not (PCRE.matches mac_ip_re addr) then
        error (f_"cannot parse --mac ip %s: doesn’t look like “%s” \
                  is an IP address") what addr
    in
    error_unless_ip_addr "ipaddr" if_ip_address;
    let if_default_gateway =
      match if_default_gateway with "" -> None | gw -> Some gw in
    Option.iter (error_unless_ip_addr "gw") if_default_gateway;
    List.iter (error_unless_ip_addr "nameserver") if_nameservers;
    let if_prefix_length =
      match if_prefix_length_str with
      | "" -> None
      | len ->
         let len =
           try int_of_string len with
           | Failure _ -> error (f_"cannot parse --mac ip prefix length field \
                                    as an integer: %s") len in
         if len < 0 || len > 128 then
           error (f_"--mac ip prefix length field is out of range");
         Some len in
    List.push_back static_ips
      { if_mac_addr; if_ip_address; if_default_gateway;
        if_prefix_length; if_nameservers }
  in

  let root_choice = ref default_root_choice in
  let set_root_choice = Types.set_root_choice root_choice in

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

  let output_conn = ref None in
  let output_format = ref None in
  let output_name = ref None in
  let output_password = ref None in
  let output_storage = ref None in

  (* Other options that we handle here. *)
  let print_source = ref false in

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

  let output_modes =
    Select_output.output_modes |>
    List.map Select_output.string_of_output_mode |>
    String.concat "|" in
  let output_mode = ref None in
  let set_output_mode mode =
    if !output_mode <> None then
      error (f_"%s option used more than once on the command line") "-o";
    output_mode := Some (Select_output.output_mode_of_string mode)
  in

  (* Options that are errors. *)
  let in_place_error _ =
    error (f_"The --in-place option has been replaced by the \
              ‘virt-v2v-in-place’ program")
  in

  let argspec = [
    [ L"bandwidth" ], Getopt.String ("bps", set_string_option_once "--bandwidth" bandwidth),
                                    s_"Set bandwidth to bits per sec";
    [ L"bandwidth-file" ], Getopt.String ("filename", set_string_option_once "--bandwidth-file" bandwidth_file),
                                    s_"Set bandwidth dynamically from file";
    [ S 'b'; L"bridge" ], Getopt.String ("in:out", add_bridge),
      s_"Map bridge ‘in’ to ‘out’";
    [ L"block-driver" ], Getopt.String ("driver", set_string_option_once "--block-driver" block_driver),
                                    s_"Prefer 'virtio-blk' or 'virtio-scsi'";
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
    [ L"in-place" ], Getopt.Unit in_place_error,
                                    s_"Use virt-v2v-in-place instead";
    [ L"mac" ],      Getopt.String ("mac:network|bridge|ip:out", add_mac),
      s_"Map NIC to network or bridge or assign static IP";
    [ S 'n'; L"network" ], Getopt.String ("in:out", add_network),
      s_"Map network ‘in’ to ‘out’";
    [ S 'o' ],       Getopt.String (output_modes, set_output_mode),
      s_"Set output mode (default: libvirt)";
    [ M"oa" ],       Getopt.String ("sparse|preallocated", set_output_alloc),
                                    s_"Set output allocation mode";
    [ M"oc" ],       Getopt.String ("uri", set_string_option_once "-oc" output_conn),
                                    s_"Output hypervisor connection";
    [ M"of" ],       Getopt.String ("raw|qcow2", set_string_option_once "-of" output_format),
                                    s_"Set output format";
    [ M"on" ],       Getopt.String ("name", set_string_option_once "-on" output_name),
                                    s_"Rename guest when converting";
    [ M"oo" ],       Getopt.String ("option[=value]", set_output_option),
                                    s_"Set option for output mode";
    [ M"op" ],       Getopt.String ("filename", set_string_option_once "-op" output_password),
                                    s_"Use password from file to connect to output hypervisor";
    [ M"os" ],       Getopt.String ("storage", set_string_option_once "-os" output_storage),
                                    s_"Set output storage location";
    [ L"parallel" ], Getopt.Set_int ("N", parallel),
      s_"Run up to N instances of nbdcopy in parallel";
    [ L"print-source" ], Getopt.Set print_source,
      s_"Print source and stop";
    [ L"root" ],     Getopt.String ("ask|... ", set_root_choice),
      s_"How to choose root filesystem";
  ] in

  (* Append virt-customize options. *)
  let customize_argspec, get_customize_ops =
    Customize_cmdline.argspec ~v2v:true () in
  let customize_argspec =
    List.map (fun (spec, _, _) -> spec) customize_argspec in
  let argspec = argspec @ customize_argspec in

  let args = ref [] in
  let anon_fun s = List.push_front s args in
  let usage_msg =
    sprintf (f_"\
%s: convert a guest to use KVM

virt-v2v -ic vpx://vcenter.example.com/Datacenter/esxi -os imported esx_guest

virt-v2v -ic vpx://vcenter.example.com/Datacenter/esxi esx_guest \
         -o ovirt -os ovirt.nfs:/export_domain --network ovirtmgmt

virt-v2v -i libvirtxml guest-domain.xml -o local -os /var/tmp

virt-v2v -i disk disk.img -o local -os /var/tmp

virt-v2v -i disk disk.img -o glance

There is a companion front-end called \"virt-p2v\" which comes as an
ISO or CD image that can be booted on physical machines.

A short summary of the options is given below.  For detailed help please
read the man page virt-v2v(1).
")
      prog in

  let opthandle = create_standard_options argspec ~anon_fun ~key_opts:true
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

  (* Create the temporary directory to control conversion. *)
  let v2vdir = create_v2v_directory () in

  (* Dereference the arguments. *)
  let args = List.rev !args in
  let block_driver =
    match !block_driver with
    | None | Some "virtio-blk" -> Virtio_blk
    | Some "virtio-scsi" -> Virtio_SCSI
    | Some driver ->
       error (f_"unknown block driver ‘--block-driver %s’") driver in
  let customize_ops = get_customize_ops () in
  let input_conn = !input_conn in
  let input_mode = !input_mode in
  let input_transport =
    match !input_transport with
    | None -> None
    | Some "ssh" -> Some Input.SSH
    | Some "vddk" -> Some Input.VDDK
    | Some transport ->
       error (f_"unknown input transport ‘-it %s’") transport in
  let output_alloc =
    match !output_alloc with
    | `Not_set | `Sparse -> Types.Sparse
    | `Preallocated -> Types.Preallocated in
  let output_mode = !output_mode in
  let output_name = !output_name in
  let parallel = !parallel in
  if parallel < 1 then
    error (f_"--parallel parameter must be >= 1");
  let print_source = !print_source in
  let root_choice = !root_choice in
  let static_ips = !static_ips in

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
      pr "io/oo\n";
      pr "mac-option\n";
      pr "bandwidth-option\n";
      pr "mac-ip-option\n";
      pr "parallel-option\n";
      pr "customize-ops\n";
      Select_input.input_modes |>
        List.map Select_input.string_of_input_mode |>
        List.iter (pr "input:%s\n");
      Select_output.output_modes |>
        List.map Select_output.string_of_output_mode |>
        List.iter (pr "output:%s\n");
      pr "convert:linux\n";
      pr "convert:windows\n";
      List.iter (pr "ovf:%s\n") Create_ovf.ovf_flavours;
      exit 0
   | _, _ -> ()
  );

  (* Select the input module. *)
  let (module Input_module) =
    Select_input.select_input input_mode input_conn input_transport in

  let input_options = {
    Input.bandwidth =
      (match !bandwidth, !bandwidth_file with
       | None, None -> None
       | Some rate, None -> Some (StaticBandwidth rate)
       | rate, Some filename -> Some (DynamicBandwidth (rate, filename)));
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

  (* Select the output module. *)
  let (module Output_module) = Select_output.select_output output_mode in

  let output_options = {
    Output.output_alloc = output_alloc;
    output_conn = !output_conn;
    output_format = Option.value ~default:"raw" !output_format;
    output_name = output_name;
    output_options = !output_options;
    output_password = !output_password;
    output_storage = !output_storage
  } in

  (* If -oo ? then we want to query output options supported in this mode. *)
  if !oo_query then (
    Output_module.query_output_options ();
    exit 0
  );

  (* XXX This is a hack for -o ovirt and -o vdsm where we must remove
   * the serial console for Linux conversions.  We don't do this for
   * -o ovirt-upload, even though we probably should, which would
   * indicate that actually oVirt is fine if we don't do this.
   *)
  let remove_serial_console =
    match output_mode with
    | Some Select_output.OVirt | Some VDSM -> true
    | _ -> false in

  (* Get the conversion options. *)
  let conv_options = {
    Convert.block_driver = block_driver;
    keep_serial_console = not remove_serial_console;
    ks = opthandle.ks;
    network_map;
    root_choice;
    static_ips;
    customize_ops;
  } in

  (* Before starting the input module, check there is sufficient
   * free space in the temporary directory on the host.
   *)
  check_host_free_space ();

  (* Start the input module (runs an NBD server in the background). *)
  message (f_"Setting up the source: %s")
    (Input_module.to_string input_options args);
  let source, input_disks = Input_module.setup v2vdir input_options args in

  (* If --print-source then print the source metadata and exit. *)
  if print_source then (
    printf (f_"Source guest information (--print-source option):\n");
    printf "\n";
    printf "%s\n" (Types.string_of_source source);
    exit 0
  );

  (* Check and parse the output options on the command line.
   * Do this before starting conversion to catch errors early, but
   * we have to do it after creating the source above.
   *)
  let output_poptions = Output_module.parse_options output_options source in

  (* Do the conversion. *)
  with_open_out (v2vdir // "convert") (fun _ -> ());
  let inspect, target_meta = Convert.convert input_disks conv_options source in
  unlink (v2vdir // "convert");

  (* Start the output module (runs an NBD server in the background). *)
  message (f_"Setting up the destination: %s")
    (Output_module.to_string output_options);
  let output_t, output_disks =
    Output_module.setup v2vdir output_poptions source input_disks in

  (* Debug the v2vdir. *)
  if verbose () then (
    let cmd = sprintf "ls -alZ %s 1>&2" (quote v2vdir) in
    ignore (Sys.command cmd)
  );

  (* Do the copy. *)
  with_open_out (v2vdir // "copy") (fun _ -> ());

  (* Get the list of disks and corresponding NBD URIs. *)
  let disks =
    List.combine input_disks output_disks |>
    List.mapi (
      fun i (input_uri, output_uri) ->
        let input_uri = NBD_URI.to_uri input_uri
        and output_uri = NBD_URI.to_uri output_uri in
        (i, input_uri, output_uri)
    ) in
  let nr_disks = List.length disks in

  (* Copy the disks. *)
  let disks = ref disks in
  let nbdcopy_pids = ref [] in
  let rec copy_loop () =
    if List.length !nbdcopy_pids < parallel && !disks <> [] then (
      (* Schedule another nbdcopy process. *)
      let i, input_uri, output_uri = List.pop_front disks in
      message (f_"Copying disk %d/%d") (i+1) nr_disks;

      let request_size = Output_module.request_size in

      (* In verbose mode print some information about each
       * side of the pipeline.
       *)
      if verbose () then (
        let input_info = nbdinfo ~content:true input_uri
        and output_info = nbdinfo ~content:false output_uri in
        eprintf "info: input disk %d/%d:\n" (i+1) nr_disks;
        eprintf "%s\n" (String.concat "\n" input_info);
        eprintf "info: output disk %d/%d:\n" (i+1) nr_disks;
        eprintf "%s\n" (String.concat "\n" output_info);
        flush Stdlib.stderr
      );

      let pid = nbdcopy ?request_size output_alloc input_uri output_uri in
      List.push_front pid nbdcopy_pids;

      copy_loop ();
    )
    else if !nbdcopy_pids <> [] then (
      (* Wait for one nbdcopy instance to exit. *)
      let pid, status = wait () in
      (* If this internal error turns up in real world scenarios then
       * we may need to change the [wait] above so it only waits on
       * the nbdcopy PIDs.
       *)
      if not (List.mem pid !nbdcopy_pids) then
        error (f_"internal error: wait returned unexpected \
                  process ID %d status \"%s\"")
          pid (string_of_process_status status);
      nbdcopy_pids := List.filter ((<>) pid) !nbdcopy_pids;
      (match status with
       | WEXITED 0 -> copy_loop ()
       | WEXITED _ | WSIGNALED _ | WSTOPPED _ ->
          error "nbdcopy %s" (string_of_process_status status)
      );
    )
  in
  copy_loop ();
  assert (!disks == []);
  assert (!nbdcopy_pids == []);

  (* End of copying phase. *)
  unlink (v2vdir // "copy");

  (* Do the finalization step. *)
  message (f_"Creating output metadata");
  Output_module.finalize v2vdir output_poptions output_t output_disks
    source inspect target_meta;

  message (f_"Finishing off");
  (* As the last thing, write a file indicating success before
   * we exit (so before we kill the helpers).  The helpers may
   * use the presence or absence of the file to determine if
   * on-success or on-fail cleanup is required.
   *)
  with_open_out (v2vdir // "done") (fun _ -> ())

(* Conversion can fail or hang if there is insufficient free space in
 * the large temporary directory.  Some input modules use large_tmpdir
 * to unpack OVAs or store qcow2 overlays and some output modules
 * use it to store temporary files.  In addition the  500 MB guestfs
 * appliance may be created there.  (RHBZ#1316479, RHBZ#2051394)
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

(* Start nbdcopy as a background process, returning the PID. *)
and nbdcopy ?request_size output_alloc input_uri output_uri =
  (* XXX It's possible that some output modes know whether
   * --target-is-zero which would be a useful optimization.
   *)
  let cmd = ref [] in
  List.push_back_list cmd [ Config.nbdcopy; input_uri; output_uri ];

  (match request_size with
    | None -> ()
    | Some size -> List.push_back cmd (sprintf "--request-size=%d" size)
  );
  (* Choose max requests to target an implicit buffer size of 64M. *)
  let requests =
    let target_buffer_size = 64 * 1024 * 1024 in
    let request_size =
      match request_size with
      | None -> 256 * 1024 (* default in nbdcopy 1.10+ *)
      | Some size -> size in
    min 64 (target_buffer_size / request_size) in
  List.push_back cmd (sprintf "--requests=%d" requests);

  if verbose () && nbdcopy_supports_blkhash () then
    List.push_back cmd "--blkhash";

  List.push_back cmd "--flush";
  (*List.push_back cmd "--verbose";*)

  if not (quiet ()) then List.push_back cmd "--progress";
  if output_alloc = Types.Preallocated then List.push_back cmd "--allocated";

  let args = !cmd in
  debug "%s" (stringify_args args);

  let args = Array.of_list args in
  match fork () with
  | 0 ->
     (* Child process (nbdcopy). *)
     execvp "nbdcopy" args
  | pid -> pid

(* Run nbdinfo on a URI and collect the information.  However don't
 * fail if nbdinfo is not installed since this is just used for debugging.
 *)
and nbdinfo ?(content = false) uri =
  let cmd =
    sprintf "%s%s %s ||:"
      (quote Config.nbdinfo)
      (if content then " --content" else " --no-content")
      (quote uri) in
  external_command cmd

let () = run_main_and_handle_errors main
