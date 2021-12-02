(* helper-v2v-input
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

open Printf
open Unix

open Std_utils
open Tools_utils
open Unix_utils.Env
open Common_gettext.Gettext
open Getopt.OptionName

open Types
open Utils
open Name_from_disk
open Parse_libvirt_xml

type cmdline = {
  bandwidth : Types.bandwidth option;
  input_conn : string option;
  input_format : string option;
  input_options : (string * string) list;
  input_password : string option;
  input_transport : [`SSH|`VDDK] option;
}

(* RHBZ#1570407: VMware-generated OVA files found in the wild can
 * contain hrefs referencing snapshots.  The href will be something
 * like: <File href="disk1.vmdk"/> but the actual disk will be a
 * snapshot called something like "disk1.vmdk.000000000".
 *)
let re_snapshot = PCRE.compile "\\.(\\d+)$"

(* Install a signal handler so we can clean up subprocesses on exit. *)
let cleanup_pid, cleanup_socket =
  let open Sys in
  let pids = ref [] and sockets = ref [] in
  let cleanup_pid pid = List.push_front pid pids in
  let cleanup_socket sock = List.push_front sock sockets in
  let cleanup _ =
    List.iter (fun pid -> kill pid sigterm) !pids;
    List.iter unlink !sockets;
  in
  List.iter (
    fun signl -> ignore (signal signl (Signal_handle cleanup))
  ) [ sigint; sigquit; sigterm; sighup ];
  cleanup_pid, cleanup_socket

let rec main () =
  (* Parse the command line. *)
  let set_string_option_once optname optref arg =
    match !optref with
    | Some _ ->
       error (f_"%s option used more than once on the command line") optname
    | None ->
       optref := Some arg
  in

  let input_mode = ref None in
  let set_input_mode mode =
    if !input_mode <> None then
      error (f_"%s option used more than once on the command line") "-im";
    match mode with
    | "disk" -> input_mode := Some `Disk
    | "libvirt" -> input_mode := Some `Libvirt
    | "libvirtxml" -> input_mode := Some `LibvirtXML
    | "ova" -> input_mode := Some `OVA
    | "vcenter-https" -> input_mode := Some `VCenterHTTPS
    | "vddk" -> input_mode := Some `VDDK
    | "vmx" -> input_mode := Some `VMX
    | "xen-ssh" -> input_mode := Some `XenSSH
    | s -> error (f_"unknown -im option: %s") s
  in

  let input_options = ref [] in
  let io_query = ref false in
  let set_input_option option =
    if option = "?" then io_query := true (* -io ? *)
    else (
      let kv = String.split "=" option in
      List.push_back input_options kv
    )
  in

  let bandwidth = ref None in
  let bandwidth_file = ref None in
  let input_conn = ref None in
  let input_format = ref None in
  let input_password = ref None in
  let input_transport = ref None in
  let args = ref [] in
  let anon_fun s = List.push_front s args in

  let argspec = [
    [ L"bandwidth" ], Getopt.String ("bps", set_string_option_once "--bandwidth" bandwidth),
                                    s_"Set bandwidth to bits per sec";
    [ L"bandwidth-file" ], Getopt.String ("filename", set_string_option_once "--bandwidth-file" bandwidth_file),
                                    s_"Set bandwidth dynamically from file";
    [ M"ic" ],       Getopt.String ("uri", set_string_option_once "-ic" input_conn),
                                    s_"Libvirt URI";
    [ M"if" ],       Getopt.String ("format", set_string_option_once "-if" input_format),
                                    s_"Input format";
    [ M"im" ],       Getopt.String ("disk|libvirt|libvirtxml|ova|vcenter-https|vddk|vmx|xen-ssh", set_input_mode),
                                    s_"Set input mode";
    [ M"io" ],       Getopt.String ("option[=value]", set_input_option),
                                    s_"Set option for input mode";
    [ M"ip" ],       Getopt.String ("filename", set_string_option_once "-ip" input_password),
                                    s_"Use password from file to connect to input hypervisor";
    [ M"it" ],       Getopt.String ("transport", set_string_option_once "-it" input_transport),
                                    s_"Input transport";
  ] in

  let usage_msg =
    sprintf (f_"\
%s: helper to set up virt-v2v for input

helper-v2v-input -im MODE V2VDIR [MODE SPECIFIC PARAMETERS ...]
")
      prog in
  let opthandle =
    create_standard_options argspec ~anon_fun ~program_name:true usage_msg in
  Getopt.parse opthandle.getopt;

  (* Dereference arguments. *)
  let args = List.rev !args in
  let cmdline = {
    bandwidth =
      (match !bandwidth, !bandwidth_file with
       | None, None -> None
       | Some rate, None -> Some (StaticBandwidth rate)
       | rate, Some filename -> Some (DynamicBandwidth (rate, filename)));
    input_conn = !input_conn;
    input_format = !input_format;
    input_options = !input_options;
    input_password = !input_password;
    input_transport =
      (match !input_transport with
       | None -> None
       | Some "ssh" -> Some `SSH
       | Some "vddk" -> Some `VDDK
       | Some it -> error (f_"unknown -it option, must be -it ssh|vddk"))
  } in
  let io_query = !io_query in

  (* -im option is required in this tool.  It is set by virt-v2v. *)
  let input_mode =
    match !input_mode with
    | None -> error (f_"-im parameter was not set")
    | Some im -> im in

  (* -io ? means list the valid input options for this mode. *)
  if io_query then (
    (match input_mode with
     | `Disk | `Libvirt | `LibvirtXML | `OVA | `VCenterHTTPS | `VMX | `XenSSH ->
        printf (f_"No input options can be used in this mode.\n");
     | `VDDK ->
        vddk_print_input_options ();
    );
    exit 0
  );

  (* Check -io is only used for input modes that support it. *)
  if cmdline.input_options <> [] then (
    match input_mode with
    | `Disk | `Libvirt | `LibvirtXML | `OVA | `VCenterHTTPS | `VMX | `XenSSH ->
       error (f_"no -io (input options) are allowed here")
    | `VDDK -> ()
  );

  (* The first parameter must be the V2V directory. *)
  let dir, args =
    match args with
    | [] -> error (f_"the first parameter must be the V2V directory")
    | dir :: args -> dir, args in

  (* The v2v directory must exist. *)
  if not (is_directory dir) then
    error (f_"%s does not exist or is not a directory") dir;

  (* Create the source struct and input-mode-specific data. *)
  let source, data =
    match input_mode with
    | `Disk -> disk_source cmdline args
    | `Libvirt -> libvirt_source cmdline args
    | `LibvirtXML -> libvirt_xml_source cmdline args
    | `OVA -> ova_source cmdline args
    | `VCenterHTTPS -> vcenter_https_source cmdline args
    | `VDDK -> vddk_source cmdline args
    | `VMX -> vmx_source cmdline args
    | `XenSSH -> xen_ssh_source cmdline args in

  (* Create NBD server instances. *)
  (match data with
   | `Disk data -> disk_servers dir data
   | `Libvirt data -> libvirt_servers dir data
   | `OVA data -> ova_servers dir data
   | `VCenterHTTPS data -> vcenter_https_servers dir data
   | `VDDK data -> vddk_servers dir data
   | `VMX data -> vmx_servers dir data
   | `XenSSH data -> xen_ssh_servers dir data
  );

  (* Write out the source metadata. *)
  with_open_out (dir // "source") (
    fun chan ->
      output_value chan Utils.metaversion;
      output_value chan source
  );

  (* Now everything should be running, write our PID and
   * wait until we get a signal.
   *)
  let in_pid = dir // "in.pid" in
  with_open_out in_pid (fun chan -> fprintf chan "%d" (getpid ()));
  pause ()

(*----------------------------------------------------------------------*)
(* -im disk *)

and disk_source cmdline args =
  if args = [] then
    error (f_"-i disk: expecting a disk image (filename) on the command line");

  (* Check the input files exist and are readable. *)
  List.iter (fun disk -> access disk [R_OK]) args;

  (* What name should we use for the guest?  We try to derive it from
   * the first filename passed in.  Users can override this using the
   * `-on name' option.
   *)
  let name = name_from_disk (List.hd args) in

  let s_disks =
    List.mapi (
      fun i _ -> { s_disk_id = i; s_controller = None }
    ) args in

  (* Give the guest a simple generic network interface. *)
  let s_nic = {
    s_mac = None;
    s_nic_model = None;
    s_vnet = "default";
    s_vnet_type = Network;
  } in

  let source = {
    s_hypervisor = UnknownHV;
    s_name = name;
    s_genid = None;
    s_memory = 2048L *^ 1024L *^ 1024L; (* 2048 MB *)
    s_vcpu = 1;            (* 1 vCPU is a safe default *)
    s_cpu_vendor = None;
    s_cpu_model = None;
    s_cpu_topology = None;
    s_features = [ "acpi"; "apic"; "pae" ];
    s_firmware = UnknownFirmware; (* causes virt-v2v to autodetect *)
    s_display =
      Some { s_display_type = Window; s_keymap = None; s_password = None;
             s_listen = LNoListen; s_port = None };
    s_sound = None;
    s_disks = s_disks;
    s_removables = [];
    s_nics = [s_nic];
  } in

  let input_format = detect_local_input_format cmdline args in
  source, `Disk (input_format, args)

(* For a list of local disks, try to detect the input format if
 * the [-if] option was not used on the command line.  If the
 * formats of the disks are different, that is an error.
 *)
and detect_local_input_format { input_format } filenames =
  match input_format with
  | Some fmt -> fmt
  | None ->
     let formats =
       let g = Guestfs.create () in
       List.map (Guestfs.disk_format g) filenames in

     let rec get_format = function
       | [] -> error (f_"expected >= 1 disk name on the command line")
       | [x] -> x
       | x :: y :: xs when compare x y = 0 -> get_format (y :: xs)
       | _ -> error (f_"disks on the command line have mixed formats")
     in

     get_format formats

and disk_servers dir (input_format, args) =
  (* Check nbdkit is installed. *)
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working.  It is required to use ‘-i disk’.");

  if not (Nbdkit.probe_plugin "file") then
    error (f_"nbdkit-file-plugin is not installed or not working");
  if not (Nbdkit.probe_filter "cow") then
    error (f_"nbdkit-cow-filter is not installed or not working");

  let nbdkit_config = Nbdkit.config () in

  List.iteri (
    fun i disk ->
      let socket = sprintf "%s/in%d" dir i in
      cleanup_socket socket;

      match input_format with
      | "raw" ->
         let cmd = Nbdkit.new_cmd in
         let cmd = Nbdkit.set_verbose cmd (verbose ()) in
         let cmd = Nbdkit.set_plugin cmd "file" in
         let cmd = Nbdkit.add_filter cmd "cow" in
         let cmd = Nbdkit.add_arg cmd "file" disk in
         let cmd =
           if Nbdkit.version nbdkit_config >= (1, 22, 0) then (
             let cmd = Nbdkit.add_arg cmd "fadvise" "sequential" in
             let cmd = Nbdkit.add_arg cmd "cache" "none" in
             cmd
           )
           else cmd in
         let _, pid = Nbdkit.run_unix ~socket cmd in

         (* --exit-with-parent should ensure nbdkit is cleaned
          * up when we exit, but it's not supported everywhere.
          *)
         cleanup_pid pid

      | format ->
         let cmd = QemuNBD.new_cmd in
         let cmd = QemuNBD.set_disk cmd disk in
         let cmd = QemuNBD.set_snapshot cmd true in (* protective overlay *)
         let cmd = QemuNBD.set_format cmd (Some format) in
         let _, pid = QemuNBD.run_unix ~socket cmd in
         cleanup_pid pid
  ) args

(*----------------------------------------------------------------------*)
(* -im libvirt *)

and libvirt_source cmdline args =
  let guest =
    match args with
    | [arg] -> arg
    | _ ->
       error (f_"-i libvirt: expecting a libvirt guest name on the command line") in

  (* Connect to the hypervisor. *)
  let conn =
    let auth = Libvirt_utils.auth_for_password_file
                 ?password_file:cmdline.input_password () in
    Libvirt.Connect.connect_auth ?name:cmdline.input_conn auth in

  (* Parse the libvirt XML. *)
  let source, disks, _ = parse_libvirt_domain conn guest in
  source, `Libvirt disks

and libvirt_servers dir disks =
  (* Check nbdkit is installed. *)
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working.  It is required to use ‘-i libvirt|libvirtxml’.");

  if not (Nbdkit.probe_plugin "file") then
    error (f_"nbdkit-file-plugin is not installed or not working");
  if not (Nbdkit.probe_plugin "nbd") then
    error (f_"nbdkit-nbd-plugin is not installed or not working");
  if not (Nbdkit.probe_filter "cow") then
    error (f_"nbdkit-cow-filter is not installed or not working");

  let nbdkit_config = Nbdkit.config () in

  List.iteri (
    fun i { d_format = format; d_type } ->
      let socket = sprintf "%s/in%d" dir i in
      cleanup_socket socket;

      match d_type with
      (* Forward to another NBD server using nbdkit-nbd-plugin. *)
      | NBD (hostname, port) ->
         let cmd = Nbdkit.new_cmd in
         let cmd = Nbdkit.set_verbose cmd (verbose ()) in
         let cmd = Nbdkit.set_plugin cmd "nbd" in
         let cmd = Nbdkit.add_filter cmd "cow" in
         let cmd = Nbdkit.add_arg cmd "hostname" hostname in
         let cmd = Nbdkit.add_arg cmd "port" (string_of_int port) in
         let _, pid = Nbdkit.run_unix ~socket cmd in

         (* --exit-with-parent should ensure nbdkit is cleaned
          * up when we exit, but it's not supported everywhere.
          *)
         cleanup_pid pid

      (* Forward to an HTTP/HTTPS server using nbdkit-curl-plugin. *)
      | HTTP url ->
         let cor = dir // "convert" in
         let cmd = Nbdkit_curl.create_curl ~cor url in
         let _, pid = Nbdkit.run_unix ~socket cmd in

         (* --exit-with-parent should ensure nbdkit is cleaned
          * up when we exit, but it's not supported everywhere.
          *)
         cleanup_pid pid

      | BlockDev filename | LocalFile filename ->
         match format with
         | Some "raw" ->
            let cmd = Nbdkit.new_cmd in
            let cmd = Nbdkit.set_verbose cmd (verbose ()) in
            let cmd = Nbdkit.set_plugin cmd "file" in
            let cmd = Nbdkit.add_filter cmd "cow" in
            let cmd = Nbdkit.add_arg cmd "file" filename in
            let cmd =
              if Nbdkit.version nbdkit_config >= (1, 22, 0) then (
                let cmd = Nbdkit.add_arg cmd "fadvise" "sequential" in
                let cmd = Nbdkit.add_arg cmd "cache" "none" in
                cmd
              )
              else cmd in
            let _, pid = Nbdkit.run_unix ~socket cmd in

            (* --exit-with-parent should ensure nbdkit is cleaned
             * up when we exit, but it's not supported everywhere.
             *)
            cleanup_pid pid

         (* We use qemu-nbd for all other formats including auto-detect. *)
         | _ ->
            let cmd = QemuNBD.new_cmd in
            let cmd = QemuNBD.set_disk cmd filename in
            let cmd = QemuNBD.set_snapshot cmd true in (* protective overlay *)
            let cmd = QemuNBD.set_format cmd format in
            let _, pid = QemuNBD.run_unix ~socket cmd in
            cleanup_pid pid
  ) disks

(*----------------------------------------------------------------------*)
(* -im libvirtxml *)

and libvirt_xml_source _ args =
  let xmlfile =
    match args with
    | [arg] -> arg
    | _ ->
       error (f_"-i libvirtxml: expecting a libvirt XML filename on the command line") in
  let xml = read_whole_file xmlfile in
  let source, disks = parse_libvirt_xml xml in
  source, `Libvirt disks

(*----------------------------------------------------------------------*)
(* -im ova *)

and ova_source _ args =
  let ova =
    match args with
    | [ova] -> ova
    | _ ->
       error (f_"-i ova: expecting an OVA file name on the command line") in

  (* Check the OVA file is readable. *)
  access ova [R_OK];

  (* Extract ova file. *)
  let ova_t = OVA.parse_ova ova in

  (* Extract ovf file from ova. *)
  let ovf = OVA.get_ovf_file ova_t in

  (* Extract the manifest from *.mf files in the ova. *)
  let manifest = OVA.get_manifest ova_t in

  (* Verify checksums of files listed in the manifest. *)
  List.iter (
    fun (file_ref, csum) ->
      let filename, r =
        match file_ref with
        | OVA.LocalFile filename ->
           filename, Checksums.verify_checksum csum filename
        | OVA.TarFile (tar, filename) ->
           filename, Checksums.verify_checksum csum ~tar filename in
      match r with
      | Checksums.Good_checksum -> ()
      | Checksums.Mismatched_checksum (_, actual) ->
         error (f_"-i ova: corrupt OVA: checksum of disk %s does not match manifest (actual = %s, expected = %s)")
           filename actual (Checksums.string_of_csum_t csum)
      | Checksums.Missing_file ->
         (* RHBZ#1570407: Some OVA files generated by VMware
          * reference non-existent components in the *.mf file.
          * Generate a warning and ignore it.
          *)
         warning (f_"manifest has a checksum for non-existent file %s (ignored)")
           filename
  ) manifest;

  (* Parse the ovf file. *)
  let name, memory, vcpu, cpu_topology, firmware, disks, removables, nics =
    OVF.parse_ovf_from_ova ovf in

  let name =
    match name with
    | None ->
       warning (f_"could not parse ovf:Name from OVF document");
       name_from_disk ova
    | Some name -> name in

  (* Convert the disk hrefs into qemu URIs. *)
  let qemu_uris =
    List.map (
      fun { OVF.href; compressed } ->
        let file_ref = find_file_or_snapshot ova_t href manifest in

        match compressed, file_ref with
        | false, OVA.LocalFile filename ->
           filename

        | true, OVA.LocalFile filename ->
           (* The spec allows the file to be gzip-compressed, in
            * which case we must uncompress it into a temporary.
            *)
           let new_filename =
             Filename.temp_file ~temp_dir:Utils.large_tmpdir
               "ova" ".vmdk" in
           unlink_on_exit new_filename;
           let cmd =
             sprintf "zcat %s > %s"
               (quote filename) (quote new_filename) in
           if shell_command cmd <> 0 then
             error (f_"error uncompressing %s, see earlier error messages")
               filename;
           new_filename

        | false, OVA.TarFile (tar, filename) ->
           (* This is the tar optimization. *)
           let offset, size =
             try OVA.get_tar_offet_and_size tar filename
             with
             | Not_found ->
                error (f_"file ‘%s’ not found in the ova") filename
             | Failure msg -> error (f_"%s") msg in
           (* QEMU requires size aligned to 512 bytes. This is safe because
            * tar also works with 512 byte blocks.
            *)
           let size = roundup64 size 512L in

           (* Workaround for libvirt bug RHBZ#1431652. *)
           let tar_path = absolute_path tar in

           let doc = [
               "file", JSON.Dict [
                           "driver", JSON.String "raw";
                           "offset", JSON.Int offset;
                           "size", JSON.Int size;
                           "file", JSON.Dict [
                                       "driver", JSON.String "file";
                                       "filename", JSON.String tar_path]
                         ]
             ] in
           let uri =
             sprintf "json:%s"
               (JSON.string_of_doc ~fmt:JSON.Compact doc) in
           uri

        | true, OVA.TarFile _ ->
           (* This should not happen since {!OVA} knows that
            * qemu cannot handle compressed files here.
            *)
           assert false
      ) disks in

  (* Create the source metadata. *)
  let s_disks = List.map (fun { OVF.source_disk } -> source_disk) disks in

  let source = {
    s_hypervisor = VMware;
    s_name = name;
    s_genid = None; (* XXX *)
    s_memory = memory;
    s_vcpu = vcpu;
    s_cpu_vendor = None;
    s_cpu_model = None;
    s_cpu_topology = cpu_topology;
    s_features = []; (* XXX *)
    s_firmware = firmware;
    s_display = None; (* XXX *)
    s_sound = None;
    s_disks = s_disks;
    s_removables = removables;
    s_nics = nics;
    } in

  source, `OVA qemu_uris

and ova_servers dir qemu_uris =
  (* Run qemu-nbd for each disk. *)
  List.iteri (
    fun i qemu_uri ->
      let socket = sprintf "%s/in%d" dir i in
      cleanup_socket socket;

      let cmd = QemuNBD.new_cmd in
      let cmd = QemuNBD.set_disk cmd qemu_uri in
      let cmd = QemuNBD.set_snapshot cmd true in (* protective overlay *)
      let cmd = QemuNBD.set_format cmd None in (* auto-detect format *)
      let _, pid = QemuNBD.run_unix ~socket cmd in
      cleanup_pid pid
  ) qemu_uris

and find_file_or_snapshot ova_t href manifest =
  match OVA.resolve_href ova_t href with
  | Some f -> f
  | None ->
     (* Find all files in the OVA called [<href>.\d+] *)
     let files = OVA.get_file_list ova_t in
     let snapshots =
       List.filter_map (
         function
         | OVA.LocalFile filename -> get_snapshot_if_matches href filename
         | OVA.TarFile (_, filename) -> get_snapshot_if_matches href filename
       ) files in
     (* Pick highest. *)
     let snapshots = List.sort (fun a b -> compare b a) snapshots in
     match snapshots with
     | [] -> error_missing_href href
     | snapshot::_ ->
        let href = sprintf "%s.%s" href snapshot in
        match OVA.resolve_href ova_t href with
        | None -> error_missing_href href
        | Some f -> f

(* If [filename] matches [<href>.\d+] then return [Some snapshot]. *)
and get_snapshot_if_matches href filename =
  if PCRE.matches re_snapshot filename then (
    let snapshot = PCRE.sub 1 in
    if String.is_suffix filename (sprintf "%s.%s" href snapshot) then
      Some snapshot
    else
      None
  )
  else None

and error_missing_href href =
  error (f_"-i ova: OVF references file ‘%s’ which was not found in the OVA archive") href

(*----------------------------------------------------------------------*)
(* -im vcenter-https *)

and vcenter_https_source cmdline args =
  let open Xpath_helpers in

  (* Remove proxy environment variables so curl doesn't try to use
   * them.  Using a proxy is generally a bad idea because vCenter
   * is slow enough as it is without putting another device in
   * the way (RHBZ#1354507).
   *)
  unsetenv "https_proxy";
  unsetenv "all_proxy";
  unsetenv "no_proxy";
  unsetenv "HTTPS_PROXY";
  unsetenv "ALL_PROXY";
  unsetenv "NO_PROXY";

  let guest =
    match args with
    | [arg] -> arg
    | _ ->
       error (f_"-i libvirt: expecting a libvirt guest name on the command line") in

  (* -ic must be set and it must contain a server.  This is
   * enforced by virt-v2v.
   *)
  let input_conn =
    match cmdline.input_conn with
    | Some ic -> ic
    | None ->
       error (f_"-i libvirt: expecting -ic parameter for vcenter connection") in

  let uri =
    try Xml.parse_uri input_conn
    with Invalid_argument msg ->
      error (f_"could not parse '-ic %s'.  Original error message was: %s")
        input_conn msg in

  let server =
    match uri with
    | { Xml.uri_server = Some server } -> server
    | { Xml.uri_server = None } ->
       error (f_"-i libvirt: expecting -ic parameter to contain vcenter server name") in

  (* Connect to the hypervisor. *)
  let conn =
    let auth = Libvirt_utils.auth_for_password_file
                 ?password_file:cmdline.input_password () in
    Libvirt.Connect.connect_auth ~name:input_conn auth in

  (* Parse the libvirt XML. *)
  let source, disks, xml = parse_libvirt_domain conn guest in

  (* Find the <vmware:datacenterpath> element from the XML.  This
   * was added in libvirt >= 1.2.20.
   *)
  let dcPath =
    let doc = Xml.parse_memory xml in
    let xpathctx = Xml.xpath_new_context doc in
    Xml.xpath_register_ns xpathctx
      "vmware" "http://libvirt.org/schemas/domain/vmware/1.0";
    match xpath_string xpathctx "/domain/vmware:datacenterpath" with
    | Some dcPath -> dcPath
    | None ->
       error (f_"vcenter: <vmware:datacenterpath> was not found in the XML.  You need to upgrade to libvirt ≥ 1.2.20.") in

  source, `VCenterHTTPS (dcPath, uri, server, disks,
                         cmdline.bandwidth, cmdline.input_password)

and vcenter_https_servers dir
                          (dcPath, uri, server, disks,
                           bandwidth, input_password) =
  List.iteri (
    fun i { d_format = format; d_type } ->
      let socket = sprintf "%s/in%d" dir i in
      cleanup_socket socket;

      match d_type with
      | BlockDev _ | NBD _ | HTTP _ -> (* These should never happen? *)
         assert false

      | LocalFile path ->
         let cor = dir // "convert" in
         let pid = VCenter.start_nbdkit_for_path ?bandwidth ~cor
                     ?password_file:input_password
                     dcPath uri server path socket in
         cleanup_pid pid
  ) disks

(*----------------------------------------------------------------------*)
(* -im vddk *)

and vddk_print_input_options () =
  printf (f_"Input options (-io) which can be used with -it vddk:

  -io vddk-thumbprint=xx:xx:xx:...
                               VDDK server thumbprint (required)

All other settings are optional:

  -io vddk-config=FILE         VDDK configuration file
  -io vddk-cookie=COOKIE       VDDK cookie
  -io vddk-libdir=LIBDIR       VDDK library parent directory
  -io vddk-nfchostport=PORT    VDDK nfchostport
  -io vddk-port=PORT           VDDK port
  -io vddk-snapshot=SNAPSHOT-MOREF
                               VDDK snapshot moref
  -io vddk-transports=MODE:MODE:..
                               VDDK transports

Refer to nbdkit-vddk-plugin(1) and the VDDK documentation for further
information on these settings.
");

and vddk_source cmdline args =
  let open Xpath_helpers in

  (* Check there are no input options we don't understand.
   * Also removes the "vddk-" prefix from the internal list.
   *)
  let vddk_option_keys =
    [ "config";
      "cookie";
      "libdir";
      "nfchostport";
      "port";
      "snapshot";
      "thumbprint";
      "transports" ] in

  let options =
    List.map (
      fun (key, value) ->
      let error_invalid_key () =
        error (f_"-it vddk: ‘-io %s’ is not a valid input option") key
      in
      if not (String.is_prefix key "vddk-") then error_invalid_key ();
      let key = String.sub key 5 (String.length key-5) in
      if not (List.mem key vddk_option_keys) then error_invalid_key ();
      (key, value)
    ) cmdline.input_options in

  (* Check no option appears more than once. *)
  let keys = List.map fst options in
  if List.length keys <> List.length (List.sort_uniq compare keys) then
    error (f_"-it vddk: duplicate -io options on the command line");

  (* thumbprint is mandatory. *)
  if not (List.mem_assoc "thumbprint" options) then
    error (f_"You must pass the ‘-io vddk-thumbprint’ option with the SSL thumbprint of the VMware server.  To find the thumbprint, see the nbdkit-vddk-plugin(1) manual.  See also the virt-v2v-input-vmware(1) manual.");

  (* Get the guest name. *)
  let guest =
    match args with
    | [arg] -> arg
    | _ ->
       error (f_"-i libvirt: expecting a libvirt guest name on the command line") in

  (* -ic must be set and it must contain a server.  This is
   * enforced by virt-v2v.
   *)
  let input_conn =
    match cmdline.input_conn with
    | Some ic -> ic
    | None ->
       error (f_"-i libvirt: expecting -ic parameter for vcenter connection") in

  let uri =
    try Xml.parse_uri input_conn
    with Invalid_argument msg ->
      error (f_"could not parse '-ic %s'.  Original error message was: %s")
        input_conn msg in

  (* Connect to the hypervisor. *)
  let conn =
    let auth = Libvirt_utils.auth_for_password_file
                 ?password_file:cmdline.input_password () in
    Libvirt.Connect.connect_auth ~name:input_conn auth in

  (* Parse the libvirt XML. *)
  let source, disks, xml = parse_libvirt_domain conn guest in

  (* Find the <vmware:moref> element from the XML.  This was added
   * in libvirt >= 3.7 and is required.
   *)
  let moref =
    let doc = Xml.parse_memory xml in
    let xpathctx = Xml.xpath_new_context doc in
    Xml.xpath_register_ns xpathctx
      "vmware" "http://libvirt.org/schemas/domain/vmware/1.0";
    let xpath_string = xpath_string xpathctx in
    match xpath_string "/domain/vmware:moref" with
    | Some moref -> moref
    | None ->
       error (f_"<vmware:moref> was not found in the output of ‘virsh dumpxml \"%s\"’.  The most likely reason is that libvirt is too old, try upgrading libvirt to ≥ 3.7.") guest in

  source, `VDDK (uri, options, moref, disks,
                 cmdline.bandwidth, input_conn, cmdline.input_password)

and vddk_servers dir
                 (uri, options, moref, disks,
                 bandwidth, input_conn, input_password) =
  (* It probably never happens that the server name can be missing
   * from the libvirt URI, but we need a server name to pass to
   * nbdkit, so ...
   *)
  let server =
    match uri.Xml.uri_server with
    | Some server -> server
    | None ->
       error (f_"‘-ic %s’ URL does not contain a host name field") input_conn in

  let user = uri.Xml.uri_user in

  let config =
    try Some (List.assoc "config" options) with Not_found -> None in
  let cookie =
    try Some (List.assoc "cookie" options) with Not_found -> None in
  let libdir =
    try Some (List.assoc "libdir" options) with Not_found -> None in
  let nfchostport =
    try Some (List.assoc "nfchostport" options) with Not_found -> None in
  let port =
    try Some (List.assoc "port" options) with Not_found -> None in
  let snapshot =
    try Some (List.assoc "snapshot" options) with Not_found -> None in
  let thumbprint =
    try List.assoc "thumbprint" options
    with Not_found -> assert false (* checked above *) in
  let transports =
    try Some (List.assoc "transports" options) with Not_found -> None in

  (* Create an nbdkit instance for each disk. *)
  List.iteri (
    fun i { d_format = format; d_type } ->
      let socket = sprintf "%s/in%d" dir i in
      cleanup_socket socket;

      match d_type with
      | BlockDev _ | NBD _ | HTTP _ -> (* These should never happen? *)
         assert false

      | LocalFile path ->
         (* The <source file=...> attribute returned by the libvirt
          * VMX driver looks like "[datastore] path".  We can use it
          * directly as the nbdkit file= parameter, and it is passed
          * directly in this form to VDDK.
          *)
         let nbdkit =
           let cor = dir // "convert" in
           Nbdkit_vddk.create_vddk ?bandwidth ?config ?cookie ~cor
             ?libdir ~moref
             ?nfchostport ?password_file:input_password ?port
             ~server ?snapshot ~thumbprint ?transports ?user
             path in
         let _, pid = Nbdkit.run_unix ~socket nbdkit in
         cleanup_pid pid
  ) disks

(*----------------------------------------------------------------------*)
(* -im vmx *)

and vmx_source cmdline args =
  let open Parse_domain_from_vmx in

  let vmx_source =
    match args with
    | [arg] ->
       let input_transport =
         match cmdline.input_transport with
         | None -> None
         | Some `SSH -> Some `SSH
         | Some `VDDK ->
            error (f_"-i vmx: cannot use -it vddk in this input mode") in
       vmx_source_of_arg input_transport arg
    | _ ->
       error (f_"-i vmx: expecting a VMX file or ssh:// URI") in

  let source, filenames = parse_domain_from_vmx vmx_source in
  source, `VMX (vmx_source, filenames,
                cmdline.bandwidth, cmdline.input_password)

and vmx_servers dir (vmx_source, filenames, bandwidth, input_password) =
  let open Parse_domain_from_vmx in

  match vmx_source with
  | File vmx_filename ->
     (* Local file in VMDK format, use qemu-nbd. *)
     List.iteri (
       fun i filename ->
         let socket = sprintf "%s/in%d" dir i in
         cleanup_socket socket;

         let cmd = QemuNBD.new_cmd in
         let cmd =
           QemuNBD.set_disk cmd
             (absolute_path_from_other_file vmx_filename filename) in
         let cmd = QemuNBD.set_snapshot cmd true in (* protective overlay *)
         let cmd = QemuNBD.set_format cmd (Some "vmdk") in
         let _, pid = QemuNBD.run_unix ~socket cmd in
         cleanup_pid pid
     ) filenames

  | SSH uri ->
     List.iteri (
       fun i filename ->
         let socket = sprintf "%s/in%d" dir i in
         cleanup_socket socket;

         let vmx_path = path_of_uri uri in
         let abs_path = absolute_path_from_other_file vmx_path filename in
         let format = "vmdk" in

         (* XXX This is a hack to work around qemu / VMDK limitation
          *   "Cannot use relative extent paths with VMDK descriptor file"
          * We can remove this if the above is fixed.
          *)
         let abs_path, format =
           let flat_vmdk =
             PCRE.replace (PCRE.compile "\\.vmdk$") "-flat.vmdk" abs_path in
           if remote_file_exists uri flat_vmdk then (flat_vmdk, "raw")
           else (abs_path, format) in

         (* XXX In virt-v2v 1.42+ importing from VMX over SSH
          * was broken if the -flat.vmdk file did not exist.
          * It is still broken here.
          *)
         ignore format;

         let server = server_of_uri uri in
         let port = Option.map string_of_int (port_of_uri uri) in
         let user = uri.Xml.uri_user in
         let password =
           match input_password with
           | None -> Nbdkit_ssh.NoPassword
           | Some ip -> Nbdkit_ssh.PasswordFile ip in

         let cor = dir // "convert" in
         let nbdkit = Nbdkit_ssh.create_ssh ?bandwidth ~cor ~password ~server
                        ?port ?user abs_path in
         let _, pid = Nbdkit.run_unix ~socket nbdkit in
         cleanup_pid pid
     ) filenames

(* The filename can be an absolute path, but is more often a
 * path relative to the location of the vmx file (which might
 * be remote over SSH).
 *)
and absolute_path_from_other_file other_filename filename =
  if not (Filename.is_relative filename) then filename
  else (Filename.dirname (absolute_path other_filename)) // filename

(*----------------------------------------------------------------------*)
(* -im xen-ssh *)

and xen_ssh_source cmdline args =
  (* Get the guest name. *)
  let guest =
    match args with
    | [arg] -> arg
    | _ ->
       error (f_"-i libvirt: expecting a libvirt guest name on the command line") in

  (* -ic must be set. *)
  let input_conn =
    match cmdline.input_conn with
    | Some ic -> ic
    | None ->
       error (f_"-i libvirt: expecting -ic parameter for Xen over SSH connection") in

  let uri =
    try Xml.parse_uri input_conn
    with Invalid_argument msg ->
      error (f_"could not parse '-ic %s'.  Original error message was: %s")
        input_conn msg in

  (* Connect to the hypervisor. *)
  let conn =
    let auth = Libvirt_utils.auth_for_password_file
                 ?password_file:cmdline.input_password () in
    Libvirt.Connect.connect_auth ~name:input_conn auth in

  (* Parse the libvirt XML. *)
  let source, disks, _ = parse_libvirt_domain conn guest in

  source, `XenSSH (disks, uri,
                   cmdline.bandwidth, input_conn, cmdline.input_password)

and xen_ssh_servers dir (disks, uri, bandwidth, input_conn, input_password) =
  let server =
    match uri.Xml.uri_server with
    | Some server -> server
    | None ->
       error (f_"‘-ic %s’ URL does not contain a host name field") input_conn in

  let port =
    match uri.uri_port with
    | 0 | 22 -> None
    | i -> Some (string_of_int i) in

  let user = uri.uri_user in

  let password =
    match input_password with
    | None -> Nbdkit_ssh.NoPassword
    | Some ip -> Nbdkit_ssh.PasswordFile ip in

  (* Create an nbdkit instance for each disk. *)
  List.iteri (
    fun i { d_format = format; d_type } ->
      let socket = sprintf "%s/in%d" dir i in
      cleanup_socket socket;

      match d_type with
      | BlockDev _ | NBD _ | HTTP _ -> (* These should never happen? *)
         assert false

      | LocalFile path ->
         let cor = dir // "convert" in
         let nbdkit = Nbdkit_ssh.create_ssh ?bandwidth ~cor ~password
                        ?port ~server ?user path in
         let _, pid = Nbdkit.run_unix ~socket nbdkit in
         cleanup_pid pid
  ) disks

let () = run_main_and_handle_errors main
