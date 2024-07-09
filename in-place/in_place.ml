(* virt-v2v-in-place
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

open Create_inspector_xml

type output_xml_option =
  | No_output_xml | Output_xml_to_stdout
  | Output_xml_to_file of string

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

  let memsize = ref None in
  let set_memsize arg = memsize := Some arg in
  let smp = ref None in
  let set_smp arg = smp := Some arg in

  let network_map = Networks.create () in

  let output_xml = ref No_output_xml in
  let set_output_xml_option filename =
    if filename = "-" then output_xml := Output_xml_to_stdout
    else output_xml := Output_xml_to_file filename
  in

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
        error (f_"cannot parse --mac ip %s: doesn’t look like “%s” is \
                  an IP address") what addr
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
           | Failure _ -> error (f_"cannot parse --mac ip prefix length \
                                    field as an integer: %s") len in
         if len < 0 || len > 128 then
           error (f_"--mac ip prefix length field is out of range");
         Some len in
    List.push_back static_ips
      { if_mac_addr; if_ip_address; if_default_gateway;
        if_prefix_length; if_nameservers }
  in

  let root_choice = ref default_root_choice in
  let set_root_choice = Types.set_root_choice root_choice in

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

  let argspec = ref [
    [ S 'b'; L"bridge" ], Getopt.String ("in:out", add_bridge),
                                    s_"Map bridge ‘in’ to ‘out’";
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
    [ L"mac" ],      Getopt.String ("mac:network|bridge|ip:out", add_mac),
                                    s_"Map NIC to network or bridge or assign static IP";
    [ S 'm'; L"memsize" ], Getopt.Int ("mb", set_memsize),
                                    s_"Set memory size";
    [ S 'n'; L"network" ], Getopt.String ("in:out", add_network),
                                    s_"Map network ‘in’ to ‘out’";
    [ S 'O' ],       Getopt.String ("output.xml", set_output_xml_option),
                                    s_"Set the output filename";
    [ L"print-source" ], Getopt.Set print_source,
                                    s_"Print source and stop";
    [ L"root" ],     Getopt.String ("ask|... ", set_root_choice),
                                    s_"How to choose root filesystem";
    [ L"smp" ],      Getopt.Int ("vcpus", set_smp),
                                    s_"Set number of vCPUs";
  ] in

  if Config.enable_block_driver then
    List.push_front
      ([ L"block-driver" ],
       Getopt.String ("driver", set_string_option_once "--block-driver" block_driver),
       s_"Prefer 'virtio-blk' or 'virtio-scsi'")
      argspec;

  (* Append virt-customize options. *)
  let customize_argspec, get_customize_ops =
    Customize_cmdline.argspec ~v2v:true () in
  let customize_argspec =
    List.map (fun (spec, _, _) -> spec) customize_argspec in
  List.push_back_list argspec customize_argspec;
  let argspec = !argspec in

  let args = ref [] in
  let anon_fun s = List.push_front s args in
  let usage_msg =
    sprintf (f_"\
%s: convert a guest to use KVM in-place

Note this program modifies the guest in-place with no backup.
Normally you should use virt-v2v.

virt-v2v-in-place -i libvirtxml guest-domain.xml

virt-v2v-in-place -i disk disk.img

A short summary of the options is given below.  For detailed help please
read the man page virt-v2v-in-place(1).
")
      prog in

  let opthandle = create_standard_options argspec ~anon_fun ~key_opts:true ~machine_readable:true usage_msg in
  Getopt.parse opthandle.getopt;

  warning "virt-v2v-in-place is NOT SUPPORTED for command line use. \
           It is almost always better to use virt-v2v instead of this tool.";

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
  let block_driver =
    match !block_driver with
    | None | Some "virtio-blk" -> Virtio_blk
    | Some "virtio-scsi" -> Virtio_SCSI
    | Some driver ->
       error (f_"unknown block driver ‘--block-driver %s’") driver in
  let customize_ops = get_customize_ops () in
  let input_conn = !input_conn in
  let input_mode = !input_mode in
  let memsize = !memsize in
  let output_xml = !output_xml in
  let print_source = !print_source in
  let root_choice = !root_choice in
  let smp = !smp in
  let static_ips = !static_ips in

  (* No arguments and machine-readable mode?  Print out some facts
   * about what this binary supports.
   *)
  (match args, machine_readable () with
   | [], Some { pr } ->
      pr "virt-v2v-in-place\n";
      pr "virt-v2v-in-place-2.0\n";
      pr "libguestfs-rewrite\n";
      pr "colours-option\n";
      pr "in-place\n";
      pr "io\n";
      pr "mac-option\n";
      pr "mac-ip-option\n";
      pr "customize-ops\n";
      pr "output-xml-option\n";
      if Config.enable_block_driver then
        pr "block-driver-option\n";
      Select_input.input_modes |>
        List.map Select_input.string_of_input_mode |>
        List.iter (pr "input:%s\n");
      pr "convert:linux\n";
      pr "convert:windows\n";
      List.iter (pr "ovf:%s\n") Create_ovf.ovf_flavours;
      exit 0
   | _, _ -> ()
  );

  (* Select the input module. *)
  let (module Input_module) =
    Select_input.select_input
      ~allow_remote:false (* forbid remote inputs *)
      input_mode input_conn
      None (* no -it option *) in

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
    input_transport = None;
    read_only = false; (* No protective overlay on the source. *)
  } in

  (* If -io ? then we want to query input options supported in this mode. *)
  if !io_query then (
    Input_module.query_input_options ();
    exit 0
  );

  (* Get the conversion options. *)
  let conv_options = {
    Convert.block_driver = block_driver;
    keep_serial_console = true;
    ks = opthandle.ks;
    memsize;
    network_map;
    root_choice;
    smp;
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

  (* Do the conversion. *)
  with_open_out (v2vdir // "convert") (fun _ -> ());
  let inspect, target_meta = Convert.convert input_disks conv_options source in
  unlink (v2vdir // "convert");

  (* Debug the v2vdir. *)
  if verbose () then (
    let cmd = sprintf "ls -alZ %s 1>&2" (quote v2vdir) in
    ignore (Sys.command cmd)
  );

  (* Write the post-conversion metadata, if asked. *)
  let chan =
    match output_xml with
    | No_output_xml -> None
    | Output_xml_to_stdout -> Some Stdlib.stdout
    | Output_xml_to_file filename -> Some (open_out filename) in
  Option.iter (
    fun chan ->
      let doc = create_inspector_xml input_disks inspect target_meta in
      DOM.doc_to_chan chan doc;
      Stdlib.flush chan
  ) chan;

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
    error (f_"insufficient free space in the conversion server temporary \
              directory %s (%s).\n\nEither free up space in that directory, \
              or set the LIBGUESTFS_CACHEDIR environment variable to point \
              to another directory with more than 1GB of free space.\n\n\
              See also the virt-v2v(1) manual, section \"Minimum free \
              space check in the host\".")
          large_tmpdir (human_size free_space)

let () = run_main_and_handle_errors main
