(* virt-v2v-in-place
 * Copyright (C) 2009-2022 Red Hat Inc.
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

  let root_choice = ref AskRoot in
  let set_root_choice = function
    | "ask" -> root_choice := AskRoot
    | "single" -> root_choice := SingleRoot
    | "first" -> root_choice := FirstRoot
    | dev when String.is_prefix dev "/dev/" -> root_choice := RootDev dev
    | s ->
      error (f_"unknown --root option: %s") s
  in

  (* Other options that we handle here. *)
  let print_source = ref false in

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

  let argspec = [
    [ S 'b'; L"bridge" ], Getopt.String ("in:out", add_bridge),
                                    s_"Map bridge ‘in’ to ‘out’";
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
    [ L"mac" ],      Getopt.String ("mac:network|bridge|ip:out", add_mac),
                                    s_"Map NIC to network or bridge or assign static IP";
    [ S 'n'; L"network" ], Getopt.String ("in:out", add_network),
                                    s_"Map network ‘in’ to ‘out’";
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
%s: convert a guest to use KVM in-place

Note this program modifies the guest in-place with no backup.
Normally you should use virt-v2v.

virt-v2v -i libvirtxml guest-domain.xml

virt-v2v -i disk disk.img

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
  let customize_ops = get_customize_ops () in
  let input_conn = !input_conn in
  let input_mode = !input_mode in
  let print_source = !print_source in
  let root_choice = !root_choice in
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
      pr "input:disk\n";
      pr "input:libvirt\n";
      pr "input:libvirtxml\n";
      pr "input:ova\n";
      pr "input:vmx\n";
      pr "convert:linux\n";
      pr "convert:windows\n";
      List.iter (pr "ovf:%s\n") Create_ovf.ovf_flavours;
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
              error (f_"could not parse '-ic %s'.  Original error \
                        message was: %s")
                orig_uri msg in

          match server, scheme with
          | None, _
          | Some "", _       (* Not a remote URI. *)
          | Some _, None     (* No scheme? *)
          | Some _, Some "" ->
             (module Input_libvirt.Libvirt_)

          (* vCenter over https. *)
          | Some server, Some _ ->
             error (f_"virt-v2v-in-place does not support remote \
                       libvirt URIs") in

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
    Convert.block_driver = Virtio_blk;
    keep_serial_console = true;
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
  let source = Input_module.setup v2vdir input_options args in

  (* If --print-source then print the source metadata and exit. *)
  if print_source then (
    printf (f_"Source guest information (--print-source option):\n");
    printf "\n";
    printf "%s\n" (Types.string_of_source source);
    exit 0
  );

  (* Do the conversion. *)
  with_open_out (v2vdir // "convert") (fun _ -> ());
  let inspect, target_meta = Convert.convert v2vdir conv_options source in
  unlink (v2vdir // "convert");

  (* Debug the v2vdir. *)
  if verbose () then (
    let cmd = sprintf "ls -alZ %s 1>&2" (quote v2vdir) in
    ignore (Sys.command cmd)
  );

  (* XXX Should we create target metadata and if so where?
   *
   * If the input mode is libvirt, there is an argument for
   * updating the libvirt XML of the guest.  If the input
   * mode is disk, maybe we should write <guestname>.xml.
   *
   * For the moment we just ignore the output from the
   * conversion step.
   *)
  ignore (inspect, target_meta);

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
