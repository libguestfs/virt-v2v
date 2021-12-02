(* helper-v2v-output
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

open C_utils
open Std_utils
open Tools_utils
open Unix_utils
open Common_gettext.Gettext
open Getopt.OptionName
open Xpath_helpers

open Create_libvirt_xml
open Types
open Utils

type cmdline = {
  output_alloc : Types.output_allocation;
  output_conn : string option;
  output_format : string;
  output_options : (string * string) list;
  output_password : string option;
  output_storage : string option;
}

(* Common error message. *)
let error_option_cannot_be_used_in_output_mode mode opt =
  error (f_"-o %s: %s option cannot be used in this output mode") mode opt

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

  let output_mode = ref None in
  let set_output_mode mode =
    if !output_mode <> None then
      error (f_"%s option used more than once on the command line") "-om";
    match mode with
    | "disk" -> output_mode := Some `Disk
    | "glance" -> output_mode := Some `Glance
    | "json" -> output_mode := Some `Json
    | "libvirt" -> output_mode := Some `Libvirt
    | "null" -> output_mode := Some `Null
    | "openstack" -> output_mode := Some `Openstack
    | "qemu" -> output_mode := Some `Qemu
    | "rhv-upload" -> output_mode := Some `RHVUpload
    | "rhv" -> output_mode := Some `RHV
    | "vdsm" -> output_mode := Some `VDSM
    | s -> error (f_"unknown -om option: %s") s
  in

  let output_options = ref [] in
  let oo_query = ref false in
  let set_output_option option =
    if option = "?" then oo_query := true (* -oo ? *)
    else (
      let kv = String.split "=" option in
      List.push_back output_options kv
    )
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

  let output_conn = ref None in
  let output_format = ref None in
  let output_name = ref None in
  let output_password = ref None in
  let output_storage = ref None in
  let args = ref [] in
  let anon_fun s = List.push_front s args in

  let argspec = [
    [ M"oa" ],       Getopt.String ("sparse|preallocated", set_output_alloc),
                                    s_"Set output allocation mode";
    [ M"oc" ],       Getopt.String ("uri", set_string_option_once "-oc" output_conn),
                                    s_"Output hypervisor connection";
    [ M"of" ],       Getopt.String ("raw|qcow2", set_string_option_once "-of" output_format),
                                    s_"Set output format";
    [ M"om" ],       Getopt.String ("disk|glance|json|null|openstack|qemu|rhv-upload|rhv|vdsm", set_output_mode),
                                    s_"Set output mode";
    [ M"on" ],       Getopt.String ("name", set_string_option_once "-on" output_name),
                                    s_"Rename guest when converting";
    [ M"oo" ],       Getopt.String ("option[=value]", set_output_option),
                                    s_"Set option for output mode";
    [ M"op" ],       Getopt.String ("filename", set_string_option_once "-op" output_password),
                                    s_"Use password from file to connect to output hypervisor";
    [ M"os" ],       Getopt.String ("storage", set_string_option_once "-os" output_storage),
                                    s_"Set output storage location";
  ] in

  let usage_msg =
    sprintf (f_"\
%s: helper to set up virt-v2v for output

helper-v2v-output -om MODE setup V2VDIR [MODE SPECIFIC PARAMETERS]
helper-v2v-output -om MODE final V2VDIR [MODE SPECIFIC PARAMETERS]
")
      prog in
  let opthandle =
    create_standard_options argspec ~anon_fun ~program_name:true usage_msg in
  Getopt.parse opthandle.getopt;

  (* Dereference arguments. *)
  let args = List.rev !args in
  let cmdline = {
    output_alloc =
      (match !output_alloc with
       | `Not_set | `Sparse -> Types.Sparse
       | `Preallocated -> Types.Preallocated);
    output_conn = !output_conn;
    output_format = Option.default "raw" !output_format;
    output_options = !output_options;
    output_password = !output_password;
    output_storage = !output_storage
  } in
  let oo_query = !oo_query in

  (* -om option is required in this tool.  It is set by virt-v2v. *)
  let output_mode =
    match !output_mode with
    | None -> error (f_"-om parameter was not set")
    | Some om -> om in

  (* -oo ? means list the valid output options for this mode. *)
  if oo_query then (
    (match output_mode with
     | `Disk | `Libvirt | `Glance | `Null | `RHV ->
        printf (f_"No output options can be used in this mode.\n");
     | `Json -> json_print_output_options ()
     | `Openstack -> openstack_print_output_options ()
     | `Qemu -> qemu_print_output_options ()
     | `RHVUpload -> rhv_upload_print_output_options ()
     | `VDSM -> vdsm_print_output_options ()
    );
    exit 0
  );

  (* Check -oo is only used for output modes that support it. *)
  if cmdline.output_options <> [] then (
    match output_mode with
    | `Disk | `Libvirt | `Glance | `Null | `RHV ->
       error (f_"no -oo (output options) are allowed here")
    | `Json | `Openstack | `Qemu | `RHVUpload | `VDSM -> ()
  );

  (* The first parameter must be setup|final (phase), and the second
   * parameter must be the V2V directory.
   *)
  let phase, dir =
    match args with
    | [] -> error (f_"the first parameter must be the phase ‘setup’ or ‘final’: see %s --help for more information") prog
    | ["setup"; dir] -> `Setup, dir
    | ["final"; dir] -> `Final, dir
    | [phase; _] ->
       error (f_"unknown phase ‘%s’, must be ‘setup’ or ‘final’: see %s --help for more information") phase prog
    | _ -> error (f_"too many anon args passed to %s") prog in

  (* The v2v directory must exist. *)
  if not (is_directory dir) then
    error (f_"%s does not exist or is not a directory") dir;

  (* Per-mode option parsing. *)
  let output_mode =
    match output_mode with
    | `Disk -> disk_parse_options cmdline
    | `Glance -> glance_parse_options cmdline
    | `Json -> json_parse_options cmdline
    | `Libvirt -> libvirt_parse_options cmdline
    | `Null -> null_parse_options cmdline
    | `Openstack -> openstack_parse_options cmdline
    | `Qemu -> qemu_parse_options cmdline
    | `RHVUpload -> rhv_upload_parse_options cmdline
    | `RHV -> rhv_parse_options cmdline
    | `VDSM -> vdsm_parse_options cmdline in

  match phase with
  | `Setup ->
     setup dir output_name output_mode
  | `Final ->
     finalize dir output_mode

(* Setup mode. *)
and setup dir output_name output_mode =
  (* Calculate the output_name. *)
  let output_name =
    let source =
      with_open_in (dir // "source") (
        fun chan ->
          let ver = input_value chan in
          assert (ver = Utils.metaversion);
          (input_value chan : Types.source)
      ) in
    match !output_name with
    | None -> source.s_name
    | Some name -> name in

  (* Get a list of input disks and query the size of each. *)
  let disks =
    let rec loop acc i =
      let socket = sprintf "%s/in%d" dir i in
      if Sys.file_exists socket then (
        let nbd = NBD.create () in
        NBD.connect_unix nbd socket;
        let size = NBD.get_size nbd in
        (try
           NBD.shutdown nbd;
           NBD.close nbd
         with NBD.Error _ -> ());
        loop ((i, size) :: acc) (i+1)
      )
      else
        List.rev acc
    in
    loop [] 0 in

  (* Create NBD server instances for each disk. *)
  (match output_mode with
   | `Disk data ->
      disk_servers dir disks output_name data
   | `Glance data -> glance_servers dir disks output_name data
   | `Json data -> json_servers dir disks output_name data
   | `Libvirt data -> libvirt_servers dir disks output_name data
   | `Null -> null_servers dir disks output_name
   | `Openstack data -> openstack_servers dir disks output_name data
   | `Qemu data -> qemu_servers dir disks output_name data
   | `RHVUpload data -> rhv_upload_servers dir disks output_name data
   | `RHV data -> rhv_servers dir disks output_name data
   | `VDSM data -> vdsm_servers dir disks output_name data
  );

  (* Now everything should be running, write our PID and
   * wait until we get a signal.
   *)
  let out_pid = dir // "out.pid" in
  with_open_out out_pid (fun chan -> fprintf chan "%d" (getpid ()));
  pause ()

and finalize dir output_mode =
  (* Read the metadata written by other parts of virt-v2v. *)
  let source =
    with_open_in (dir // "source") (
      fun chan ->
        let ver = input_value chan in
        assert (ver = Utils.metaversion);
        (input_value chan : Types.source)
    ) in

  let inspect =
    with_open_in (dir // "inspect") (
      fun chan ->
        let ver = input_value chan in
        assert (ver = Utils.metaversion);
        (input_value chan : Types.inspect)
    ) in

  let target_meta =
    with_open_in (dir // "target_meta") (
      fun chan ->
        let ver = input_value chan in
        assert (ver = Utils.metaversion);
        (input_value chan : Types.target_meta)
    ) in

  (match output_mode with
   | `Disk data ->
      disk_finalize dir source inspect target_meta data
   | `Glance data -> glance_finalize dir source inspect target_meta data
   | `Json data -> json_finalize dir source inspect target_meta data
   | `Libvirt data -> libvirt_finalize dir source inspect target_meta data
   | `Null -> (* nothing to do *) ()
   | `Openstack data -> openstack_finalize dir source inspect target_meta data
   | `Qemu data -> qemu_finalize dir source inspect target_meta data
   | `RHVUpload data -> rhv_upload_finalize dir source inspect target_meta data
   | `RHV data -> rhv_finalize dir source inspect target_meta data
   | `VDSM data -> vdsm_finalize dir source inspect target_meta data
  ) (* match output_mode *)

(* Common code when an output mode wants to create a local file
 * with a particular format (only "raw" or "qcow2").
 *)
and output_local_file ?(changeuid = fun f -> f ())
                      output_alloc output_format filename size socket =
  (* Check nbdkit is installed and has the required plugin. *)
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working.  It is required to use ‘-o disk’.");
  if not (Nbdkit.probe_plugin "file") then
    error (f_"nbdkit-file-plugin is not installed or not working");
  let nbdkit_config = Nbdkit.config () in

  let g = Guestfs.create () in
  let preallocation =
    match output_alloc with
    | Preallocated -> Some "full"
    | Sparse -> None in
  changeuid (
    fun () -> Guestfs.disk_create ?preallocation g filename output_format size
  );

  match output_format with
  | "raw" ->
     let cmd = Nbdkit.new_cmd in
     let cmd = Nbdkit.set_verbose cmd (verbose ()) in
     let cmd = Nbdkit.set_plugin cmd "file" in
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

  | "qcow2" ->
     let cmd = QemuNBD.new_cmd in
     let cmd = QemuNBD.set_disk cmd filename in
     let cmd = QemuNBD.set_snapshot cmd false in
     let cmd = QemuNBD.set_format cmd (Some "qcow2") in
     let _, pid = QemuNBD.run_unix ~socket cmd in
     cleanup_pid pid

  | _ ->
     error (f_"output mode only supports raw or qcow2 format (format: %s)")
       output_format

(*----------------------------------------------------------------------*)
(* -om disk *)

and disk_parse_options cmdline =
  if cmdline.output_password <> None then
    error_option_cannot_be_used_in_output_mode "local" "-op";

  (* -os must be set to a directory. *)
  let output_storage =
    match cmdline.output_storage with
    | None ->
       error (f_"-o disk: output directory was not specified, use '-os /dir'")
    | Some d when not (is_directory d) ->
       error (f_"-os %s: output directory does not exist or is not a directory") d
    | Some d -> d in
  `Disk (cmdline.output_alloc, cmdline.output_format, output_storage)

and disk_servers dir disks output_name
                 (output_alloc, output_format, output_storage) =
  List.iter (
    fun (i, size) ->
      let socket = sprintf "%s/out%d" dir i in
      cleanup_socket socket;

      (* Create the actual output disk. *)
      let outdisk = disk_path output_storage output_name i in
      output_local_file output_alloc output_format outdisk size socket
  ) disks

and disk_finalize dir source inspect target_meta
                  (output_alloc, output_format, output_storage) =
  (* Convert metadata to libvirt XML. *)
  (match target_meta.target_firmware with
   | TargetBIOS -> ()
   | TargetUEFI ->
      (* XXX Can remove this method when libvirt supports
       * <loader type="efi"/> since then it will be up to
       * libvirt to check this.
       *)
      error_unless_uefi_firmware target_meta.guestcaps.gcaps_arch
  );

  (* We don't know what target features the hypervisor supports, but
   * assume a common set that libvirt supports.
   *)
  let target_features =
    match target_meta.guestcaps.gcaps_arch with
    | "i686" -> [ "acpi"; "apic"; "pae" ]
    | "x86_64" -> [ "acpi"; "apic" ]
    | _ -> [] in

  let output_name = target_meta.output_name in

  let doc = create_libvirt_xml source inspect target_meta
              target_features
              (disk_path output_storage output_name)
              output_format in

  let file = output_storage // output_name ^ ".xml" in
  with_open_out file (fun chan -> DOM.doc_to_chan chan doc);

  if verbose () then (
    eprintf "resulting local libvirt XML:\n";
    DOM.doc_to_chan Stdlib.stderr doc;
    eprintf "\n%!";
  )

(* For -om disk|qemu, return the output disk name of the i'th disk,
 * eg. 0 => /path/to/name-sda.
 *)
and disk_path os name i =
  let outdisk = sprintf "%s/%s-sd%s" os name (drive_name i) in
  absolute_path outdisk

(*----------------------------------------------------------------------*)
(* -om glance *)

and glance_parse_options cmdline =
  if cmdline.output_conn <> None then
    error_option_cannot_be_used_in_output_mode "glance" "-oc";
  if cmdline.output_password <> None then
    error_option_cannot_be_used_in_output_mode "glance" "-op";
  if cmdline.output_storage <> None then
    error_option_cannot_be_used_in_output_mode "glance" "-os";
  `Glance cmdline.output_format

and glance_servers dir disks output_name output_format =
  (* This does nothing useful except to check that the user has
   * supplied all the correct auth environment variables to make
   * 'glance' commands work as the current user.  If not then the
   * program exits early.
   *)
  if shell_command "glance image-list > /dev/null" <> 0 then
    error (f_"glance: glance client is not installed or set up correctly.  You may need to set environment variables or source a script to enable authentication.  See preceding messages for details.");

  (* When debugging, query the glance client for its version. *)
  if verbose () then (
    eprintf "version of the glance client:\n%!";
    ignore (shell_command "glance --version");
  );

  (* Although glance can slurp in a stream from stdin, qemu-nbd
   * (used for -of qcow2) cannot write to a stream.  This might
   * be possible in future with more creative use of NBD.  (XXX)
   * We save the location of the tmpdir as a symbolic link under
   * v2vdir (v2vdir/out.tmpdir) so we can complete the conversion
   * in finalization.
   *)
  let tmpdir = Mkdtemp.temp_dir ~base_dir:large_tmpdir "glance." in
  symlink tmpdir (dir // "out.tmpdir");

  (* This will write disks to the large temporary directory. *)
  List.iter (
    fun (i, size) ->
      let socket = sprintf "%s/out%d" dir i in
      cleanup_socket socket;

      (* Create the actual output disk. *)
      let outdisk = sprintf "%s/out.tmpdir/%d" dir i in
      output_local_file Sparse output_format outdisk size socket
  ) disks

and glance_finalize dir source inspect target_meta output_format =
  (* This is a symbolic link to the directory containing the
   * temporary disks.  It was created in the setup phase.
   *)
  if not (is_directory (dir // "out.tmpdir/")) then
    error (f_"%s/out.tmpdir does not exist or is not a directory") dir;

  let min_ram = source.s_memory /^ 1024L /^ 1024L in

  (* Get the image properties. *)
  let properties =
    Openstack_image_properties.create source inspect target_meta in
  let properties =
    List.flatten (
      List.map (
        fun (k, v) -> [ "--property"; sprintf "%s=%s" k v ]
      ) properties
    ) in

  (* The first disk, assumed to be the system disk, will be called
   * "guestname".  Subsequent disks, assumed to be data disks,
   * will be called "guestname-disk2" etc.  The manual strongly
   * hints you should import the data disks to Cinder.
   *)
  List.iteri (
    fun i _ ->
      let name =
        if i == 0 then target_meta.output_name
        else sprintf "%s-disk%d" target_meta.output_name (i+1) in

      let disk = sprintf "%s/out.tmpdir/%d" dir i in

      (* If glance is used with VMware then there's a vmware_disktype
       * option which allows preallocated.  However I don't believe
       * it's possible in general glance, so ignore the -oa option.
       * Since we are writing to a temporary file before copying to
       * glance, -oa preallocated just preallocates the temporary file.
       *)
      let cmd = [ "glance"; "image-create"; "--name"; name;
                  "--disk-format=" ^ output_format;
                  "--container-format=bare"; "--file"; disk;
                  "--min-ram"; Int64.to_string min_ram ] @
                  properties in
      if run_command cmd <> 0 then
        error (f_"glance: image upload to glance failed, see earlier errors");

      (* Unlink the temporary files as soon as glance has got them. *)
      try unlink disk with Unix_error _ -> ()
  ) source.s_disks;

  (* Remove the temporary directory for the large files. *)
  (try rmdir (readlink (dir // "out.tmpdir")) with Unix_error _ -> ())

(*----------------------------------------------------------------------*)
(* -om json *)

and json_print_output_options () =
  printf (f_"Output options (-oo) which can be used with -o json:

  -oo json-disks-pattern=PATTERN   Pattern for the disks.
")

and json_parse_options cmdline =
  if cmdline.output_conn <> None then
    error_option_cannot_be_used_in_output_mode "json" "-oc";
  if cmdline.output_password <> None then
    error_option_cannot_be_used_in_output_mode "json" "-op";

  let known_pattern_variables = ["DiskNo"; "DiskDeviceName"; "GuestName"] in
  let json_disks_pattern = ref None in
  List.iter (
    fun (k, v) ->
      match k with
      | "json-disks-pattern" ->
         if !json_disks_pattern <> None then
           error (f_"-o json: -oo json-disks-pattern set more than once");
         let vars =
           try Var_expander.scan_variables v
           with Var_expander.Invalid_variable var ->
             error (f_"-o json: -oo json-disks-pattern: invalid variable %%{%s}")
               var in
         List.iter (
           fun var ->
             if not (List.mem var known_pattern_variables) then
               error (f_"-o json: -oo json-disks-pattern: unhandled variable %%{%s}")
                 var
         ) vars;
         json_disks_pattern := Some v
      | k ->
         error (f_"-o json: unknown output option ‘-oo %s’") k
    ) cmdline.output_options;

  let json_disks_pattern =
    Option.default "%{GuestName}-%{DiskDeviceName}" !json_disks_pattern in

  (* -os must be set to a directory. *)
  let output_storage =
    match cmdline.output_storage with
    | None ->
       error (f_"-o json: output directory was not specified, use '-os /dir'")
    | Some d when not (is_directory d) ->
       error (f_"-os %s: output directory does not exist or is not a directory") d
    | Some d -> d in

  `Json (json_disks_pattern,
         cmdline.output_alloc, cmdline.output_format, output_storage)

and json_servers dir disks output_name
                 (json_disks_pattern,
                  output_alloc, output_format, output_storage) =
  List.iter (
    fun (i, size) ->
      let socket = sprintf "%s/out%d" dir i in
      cleanup_socket socket;

      (* Create the actual output disk. *)
      let outdisk = json_path output_storage output_name json_disks_pattern i in
      mkdir_p (Filename.dirname outdisk) 0o755;

      output_local_file output_alloc output_format outdisk size socket
  ) disks

and json_finalize dir source inspect target_meta
                  (json_disks_pattern,
                   output_alloc, output_format, output_storage) =
  let doc =
    Create_json.create_json_metadata source inspect target_meta
      (json_path output_storage target_meta.output_name json_disks_pattern)
      output_format in
  let doc_string = JSON.string_of_doc ~fmt:JSON.Indented doc in

  if verbose () then (
    eprintf "resulting JSON:\n";
    output_string Stdlib.stderr doc_string;
    eprintf "\n\n%!";
  );

  let file = output_storage // target_meta.output_name ^ ".json" in
  with_open_out file (
    fun chan ->
      output_string chan doc_string;
      output_char chan '\n'
  )

(* For -om json, return the output disk name of the i'th disk. *)
and json_path os output_name json_disks_pattern i =
  let outname =
    let vars_fn = function
      | "DiskNo" -> Some (string_of_int (i+1))
      | "DiskDeviceName" -> Some (sprintf "sd%s" (drive_name i))
      | "GuestName" -> Some output_name
      | _ -> assert false
    in
    Var_expander.replace_fn json_disks_pattern vars_fn in
  let outdisk = os // outname in
  let outdisk = absolute_path outdisk in
  outdisk

(*----------------------------------------------------------------------*)
(* -om libvirt *)

and libvirt_parse_options cmdline =
  if cmdline.output_password <> None then
    error_option_cannot_be_used_in_output_mode "libvirt" "-op";

  let conn = lazy (Libvirt.Connect.connect ?name:cmdline.output_conn ()) in

  (* -os is the name of the output pool.  It defaults to "default". *)
  let output_pool = Option.default "default" cmdline.output_storage in

  `Libvirt (conn, cmdline.output_alloc, cmdline.output_format, output_pool)

and libvirt_servers dir disks output_name
                    (conn, output_alloc, output_format, output_pool) =
  let conn = Lazy.force conn in

  (* Get the capabilities from libvirt. *)
  let capabilities_xml =
    try Libvirt.Connect.get_capabilities conn
    with
      Libvirt.Virterror { message } ->
      error (f_"cannot get libvirt hypervisor capabilities: %s")
        (Option.default "" message) in
  debug "libvirt capabilities XML:\n%s" capabilities_xml;

  (* This just checks that the capabilities XML is well-formed,
   * early so that we catch parsing errors before conversion.
   *)
  ignore (Xml.parse_memory capabilities_xml);

  (* Does the domain already exist on the target?  (RHBZ#889082) *)
  if Libvirt_utils.domain_exists conn output_name then
    error (f_"a libvirt domain called ‘%s’ already exists on the target.\n\nIf using virt-v2v directly, use the ‘-on’ option to select a different name. Or delete the existing domain on the target using the ‘virsh undefine’ command.\n\nIf using virt-p2v, select a different ‘Name’ in the ‘Target properties’. Or delete the existing domain on the target using the ‘virsh undefine’ command.")
      output_name;

  (* Connect to output libvirt instance and check that the pool exists
   * and dump out its XML.
   *)
  let pool = Libvirt_utils.get_pool conn output_pool in
  let xml = Libvirt.Pool.get_xml_desc (Libvirt.Pool.const pool) in
  let doc = Xml.parse_memory xml in
  let xpathctx = Xml.xpath_new_context doc in
  let xpath_string = xpath_string xpathctx in

  (* We can only output to a pool of type 'dir' (directory). *)
  if xpath_string "/pool/@type" <> Some "dir" then
    error (f_"-o libvirt: output pool ‘%s’ is not a directory (type='dir').  See virt-v2v-output-local(1)") output_pool;
  let target_path =
    match xpath_string "/pool/target/path/text()" with
    | None ->
       error (f_"-o libvirt: output pool ‘%s’ does not have /pool/target/path element.  See virt-v2v-output-local(1)") output_pool
    | Some dir when not (is_directory dir) ->
       error (f_"-o libvirt: output pool ‘%s’ has type='dir' but the /pool/target/path element is not a local directory.  See virt-v2v-output-local(1)") output_pool
    | Some dir -> dir in

  (* Get the name of the pool, since we have to use that
   * (and not the UUID) in the XML of the guest.
   *)
  let pool_name = Libvirt.Pool.get_name (Libvirt.Pool.const pool) in

  (* Stash the capabilities XML, since we cannot get the bits we
   * need from it until we know the guest architecture, which happens
   * after conversion.  Also stash the pool name used in the
   * final XML.
   *)
  with_open_out (dir // "out.libvirt")
    (fun chan -> output_value chan (capabilities_xml, pool_name));

  (* Set up the NBD servers. *)
  List.iter (
    fun (i, size) ->
      let socket = sprintf "%s/out%d" dir i in
      cleanup_socket socket;

      (* Create the actual output disk. *)
      let outdisk = target_path // output_name ^ "-sd" ^ (drive_name i) in
      output_local_file output_alloc output_format outdisk size socket
  ) disks

and libvirt_finalize dir source inspect target_meta
                     (conn, output_alloc, output_format, output_pool) =
  (match target_meta.target_firmware with
   | TargetBIOS -> ()
   | TargetUEFI ->
      (* XXX Can remove this method when libvirt supports
       * <loader type="efi"/> since then it will be up to
       * libvirt to check this.
       *)
      error_unless_uefi_firmware target_meta.guestcaps.gcaps_arch
  );

  let conn = Lazy.force conn in

  (* We copied directly into the final pool directory.  However we
   * have to tell libvirt.
   *)
  (try
     let pool = Libvirt_utils.get_pool conn output_pool in
     Libvirt.Pool.refresh (Libvirt.Pool.const pool)
   with
     Libvirt.Virterror { message } ->
     warning (f_"could not refresh libvirt pool ‘%s’: %s")
       output_pool (Option.default "" message)
  );

  (* Read the capabilities and pool name saved in the setup step. *)
  let capabilities_xml, pool_name =
    with_open_in (dir // "out.libvirt") (fun chan -> input_value chan) in
  let doc = Xml.parse_memory capabilities_xml in

  (* Parse the capabilities XML in order to get the supported features. *)
  let target_features =
    target_features_of_capabilities_doc doc target_meta.guestcaps.gcaps_arch in

  (* Create the metadata. *)
  let doc =
    create_libvirt_xml ~pool:pool_name source inspect target_meta
      target_features
      (fun i -> target_meta.output_name ^ "-sd" ^ (drive_name i))
      output_format in

  let tmpfile, chan = Filename.open_temp_file "v2vlibvirt" ".xml" in
  DOM.doc_to_chan chan doc;
  close_out chan;

  if verbose () then (
    eprintf "resulting XML for libvirt:\n%!";
    DOM.doc_to_chan Stdlib.stderr doc;
    eprintf "\n%!";
  );

  (* Define the domain in libvirt. *)
  (try
     ignore (Libvirt.Domain.define_xml conn (DOM.doc_to_string doc));
     (try Unix.unlink tmpfile with _ -> ())
   with
     Libvirt.Virterror { message } ->
     warning (f_"could not define libvirt domain: %s.\nThe libvirt XML is still available in ‘%s’.  Try running ‘virsh -c %s define %s’ yourself instead.")
       (Option.default "" message) tmpfile
       (Libvirt.Connect.get_uri conn) tmpfile
  )

and arch_is_sane_or_die =
  let rex = PCRE.compile ~caseless:true "^[-_a-z0-9]+$" in
  fun arch -> assert (PCRE.matches rex arch)

and target_features_of_capabilities_doc doc arch =
  let xpathctx = Xml.xpath_new_context doc in
  let expr =
    (* Check the arch is sane.  It comes from untrusted input.  This
     * avoids XPath injection below.
     *)
    arch_is_sane_or_die arch;
    (* NB: Pay attention to the square brackets.  This returns the
     * <guest> nodes!
     *)
    sprintf "/capabilities/guest[arch[@name='%s']/domain/@type='kvm']" arch in
  let obj = Xml.xpath_eval_expression xpathctx expr in

  if Xml.xpathobj_nr_nodes obj < 1 then (
    (* Old virt-v2v used to die here, but that seems unfair since the
     * user has gone through conversion before we reach here.
     *)
    warning (f_"the target hypervisor does not support a %s KVM guest") arch;
    []
  ) else (
    let node (* first matching <guest> *) = Xml.xpathobj_node obj 0 in
    Xml.xpathctx_set_current_context xpathctx node;

    (* Get guest/features/* nodes. *)
    let features = xpath_get_nodes xpathctx "features/*" in
    List.map Xml.node_name features
  )

(*----------------------------------------------------------------------*)
(* -om null *)

and null_parse_options cmdline =
  if cmdline.output_alloc <> Sparse then
    error_option_cannot_be_used_in_output_mode "null" "-oa";
  if cmdline.output_conn <> None then
    error_option_cannot_be_used_in_output_mode "null" "-oc";
  if cmdline.output_format <> "raw" then
    error_option_cannot_be_used_in_output_mode "null" "-of";
  if cmdline.output_password <> None then
    error_option_cannot_be_used_in_output_mode "null" "-op";
  if cmdline.output_storage <> None then
    error_option_cannot_be_used_in_output_mode "null" "-os";

  `Null

and null_servers dir disks output_name =
  (* Check nbdkit is installed and has the required plugin. *)
  if not (Nbdkit.is_installed ()) then
    error (f_"nbdkit is not installed or not working.  It is required to use ‘-o null’.");
  if not (Nbdkit.probe_plugin "null") then
    error (f_"nbdkit-null-plugin is not installed or not working");

  (* We only need to run one nbdkit instance (even if there is
   * more than one disk) and we can ignore the size of the inputs
   * and set the size of the output to 7E.
   *)
  let socket = sprintf "%s/out0" dir in
  cleanup_socket socket;

  let () =
    let cmd = Nbdkit.new_cmd in
    (*let cmd = Nbdkit.set_verbose cmd (verbose ()) in*)
    let cmd = Nbdkit.set_plugin cmd "null" in
    let cmd = Nbdkit.add_arg cmd "size" "7E" in
    let _, pid = Nbdkit.run_unix ~socket cmd in

    (* --exit-with-parent should ensure nbdkit is cleaned
     * up when we exit, but it's not supported everywhere.
     *)
    cleanup_pid pid in

  (* Use hard links to the same socket for the other disks. *)
  List.iter (
    fun (i, _) ->
      if i > 0 then (
        let output = sprintf "%s/out%d" dir i in
        link socket output
      )
  ) disks

(*----------------------------------------------------------------------*)
(* -om openstack *)

and openstack_print_output_options () =
  printf (f_"virt-v2v -oo server-id=<NAME|UUID> [os-*=...]

Specify the name or UUID of the conversion appliance using

  virt-v2v ... -o openstack -oo server-id=<NAME|UUID>

When virt-v2v runs it will attach the Cinder volumes to the
conversion appliance, so this name or UUID must be the name
of the virtual machine on OpenStack where virt-v2v is running.

In addition, all usual OpenStack “os-*” parameters or “OS_*”
environment variables can be used.

Openstack “--os-*” parameters must be written as “virt-v2v -oo os-*”.

For example:

  virt-v2v -oo os-username=<NAME>

                equivalent to openstack: --os-username=<NAME>
            or the environment variable: OS_USERNAME=<NAME>

  virt-v2v -oo os-project-name=<NAME>

                equivalent to openstack: --os-project-name=<NAME>
            or the environment variable: OS_PROJECT_NAME=<NAME>

The os-* parameters and environment variables are optional.
")

and openstack_parse_options cmdline =
  if cmdline.output_alloc <> Sparse || cmdline.output_format <> "raw" then
    error (f_"-o openstack mode only supports -oa sparse -of raw");

  let server_id = ref None in
  let dev_disk_by_id = ref None in
  let verify_server_certificate = ref true in
  let guest_id = ref None in
  let authentication = ref [] in
  List.iter (
    function
    | "server-id", v ->
       server_id := Some v
    | "dev-disk-by-id", v ->
       dev_disk_by_id := Some v
    | "verify-server-certificate", "" ->
       verify_server_certificate := true
    | "verify-server-certificate", v ->
       verify_server_certificate := bool_of_string v
    | "guest-id", v ->
       guest_id := Some v
    | k, v when String.is_prefix k "os-" ->
       (* Accumulate any remaining/unknown -oo os-* parameters
        * into the authentication list, where they will be
        * pass unmodified through to the openstack command.
        *)
       let opt = sprintf "--%s=%s" k v in
       authentication := opt :: !authentication
    | k, _ ->
       error (f_"-o openstack: unknown output option ‘-oo %s’") k
  ) cmdline.output_options;
  let server_id =
    match !server_id with
    | None ->
       error (f_"openstack: -oo server-id=<NAME|UUID> not present");
    | Some server_id -> server_id in
  let authentication = List.rev !authentication in
  let verify_server_certificate = !verify_server_certificate in
  let guest_id = !guest_id in
  let dev_disk_by_id = !dev_disk_by_id in

  (* Name of the openstack CLI program (on $PATH). *)
  let openstack_binary = "openstack" in

  (* The extra command line parameters derived from -oo etc. *)
  let extra_args =
    let args = ref authentication in
    Option.may (fun oc -> List.push_back args (sprintf "--os-auth-url=%s" oc))
               cmdline.output_conn;
    if not verify_server_certificate then
      List.push_back args "--insecure";
    !args in

  (* Check the openstack command exists. *)
  let error_unless_openstack_command_exists () =
    try ignore (which openstack_binary)
    with Executable_not_found _ ->
      error (f_"the ‘%s’ program is not available.  It is needed to communicate with OpenStack.")
        openstack_binary
  in
  error_unless_openstack_command_exists ();

  (* We use this convenient wrapper around [Tools_utils.run_command]
   * for two reasons: (1) Because we want to run openstack with
   * extra_args.  (2) OpenStack commands are noisy so we want to
   * direct stdout to /dev/null unless we're in verbose mode.
   *)
  let run_openstack_command args =
    let cmd = [ openstack_binary ] @ extra_args @ args in
    let stdout_fd =
      if verbose () then None
      else Some (openfile "/dev/null" [O_WRONLY] 0) in
    (* Note that run_command will close stdout_fd if defined.
     * Don't echo the whole command because it can contain passwords.
     *)
    debug "openstack [...] %s" (String.concat " " args);
    Tools_utils.run_command ~echo_cmd:false ?stdout_fd cmd
  in

  (* Similar to above, run the openstack command and capture the
   * JSON document printed by the command.  Note you must add
   * '-f json' to the args yourself.
   *)
  let run_openstack_command_capture_json args =
    let cmd = [ openstack_binary ] @ extra_args @ args in

    let json, chan = Filename.open_temp_file "v2vopenstack" ".json" in
    unlink_on_exit json;
    let fd = descr_of_out_channel chan in

    (* Note that Tools_utils.run_command closes fd.
     * Don't echo the whole command because it can contain passwords.
     *)
    debug "openstack [...] %s" (String.concat " " args);
    if Tools_utils.run_command ~echo_cmd:false ~stdout_fd:fd cmd <> 0 then
      None
    else (
      let json = JSON_parser.json_parser_tree_parse_file json in
      debug "openstack: JSON parsed as: %s"
            (JSON.string_of_doc ~fmt:JSON.Indented ["", json]);
      Some json
    )
  in

  (* Run the openstack command simply to check we can connect
   * with the provided authentication parameters/environment
   * variables.  Issuing a token should have only a tiny
   * overhead.
   *)
  let args = [ "token"; "issue" ] in
  if run_openstack_command args <> 0 then
    error (f_"openstack: precheck failed, there may be a problem with authentication, see earlier error messages");

  `Openstack (cmdline.output_storage, server_id, guest_id, dev_disk_by_id,
              run_openstack_command,
              run_openstack_command_capture_json)

and openstack_servers dir disks output_name
                      (output_storage, server_id, guest_id, dev_disk_by_id,
                       run_openstack_command,
                       run_openstack_command_capture_json) =
  (* Timeout waiting for Cinder volumes to attach to the appliance. *)
  let attach_timeout = 300 (* seconds *) in

  (* Timeout waiting for new Cinder volumes to move to "available" state.
   * We assume this could be quite a long time on backends which want
   * to preallocate the storage.
   *)
  let available_timeout = 300 (* seconds *) in

  (* Set a known description for volumes, then change it later
   * when conversion is successful.  In theory this would allow
   * some kind of garbage collection for unfinished conversions
   * in the case that virt-v2v crashes.
   *)
  let description = sprintf "virt-v2v temporary volume for %s" output_name in

  (* The list of volume IDs that we create as we go along. *)
  let volume_ids = ref [] in

  let detach_volume id =
    let args = [ "server"; "remove"; "volume"; server_id; id ] in
    ignore (run_openstack_command args)
  in

  (* Delete a cinder volume.
   *
   * This ignores errors since the only time we are doing this is on
   * the failure path.
   *)
  let delete_cinder_volume id =
    let args = [ "volume"; "delete"; id ] in
    ignore (run_openstack_command args)
  in

  (* Set up an at-exit handler so we:
   * (1) Unconditionally detach volumes.
   * (2) Delete the volumes, but only if conversion was not successful.
   *)
  at_exit (
    fun () ->
      let volume_ids = !volume_ids in
      List.iter detach_volume volume_ids;

      (* virt-v2v writes v2vdir/done on success only. *)
      let success = Sys.file_exists (dir // "done") in
      if not success then (
        (* XXX We probably need to wait for the previous
         * detach operation to complete - unclear how.
         *)
        List.iter delete_cinder_volume volume_ids;
      )
  );

  (* Create a new Cinder volume and wait for its status to change to
   * "available".  Returns the volume id.
   *)
  let create_cinder_volume name description size =
    (* Cinder volumes are allocated in increments of 1 GB.  Weird. *)
    let size_gb =
      let s = roundup64 size 1073741824L in
      let s = s /^ 1073741824L in
      Int64.to_string s in

    let args = ref [] in
    List.push_back_list args [ "volume"; "create";
                               "-f"; "json";
                               "--size"; size_gb;
                               "--description"; description;
                               "--non-bootable";
                               "--read-write" ];
    Option.may (
      fun os ->
        List.push_back_list args [ "--type"; os ]
    ) output_storage;
    List.push_back args name;

    let json =
      match run_openstack_command_capture_json !args with
      | None ->
         error (f_"openstack: failed to create a cinder volume, see earlier error messages")
      | Some json -> json in
    let id = JSON_parser.object_get_string "id" json in

    (* Wait for the volume state to change to "available". *)
    let args = [ "volume"; "show"; "-f"; "json"; id ] in
    with_timeout
      (s_"wait for cinder volume status to change to \"available\"")
      available_timeout
      (fun () ->
        match run_openstack_command_capture_json args with
        | None ->
           error (f_"openstack: failed to query cinder volume status, see earlier error messages")
        | Some json ->
           match JSON_parser.object_get_string "status" json with
           | "creating" -> None
           | "available" -> Some () (* done *)
           | status ->
              error (f_"openstack: unknown volume status \"%s\": expected \"creating\" or \"available\"") status
      );

    id
  in

  (* Create the Cinder volumes. *)
  List.iter (
    fun (i, size) ->
      (* Unclear what we should set the name to, so just make
       * something related to the guest name.  Cinder volume
       * names do not need to be unique.
       *)
      let name = sprintf "%s-sd%s" output_name (drive_name i) in

      (* Create the cinder volume. *)
      let id = create_cinder_volume name description size in
      List.push_back volume_ids id
  ) disks;

  (* Attach volume to current VM and wait for it to appear.
   * Returns the block device name.
   *)
  let attach_volume id =
    let args = [ "server"; "add"; "volume"; server_id; id ] in
    if run_openstack_command args <> 0 then
      error (f_"openstack: failed to attach cinder volume to VM, see earlier error messages");

    (* We expect the disk to appear under /dev/disk/by-id.
     *
     * In theory the serial number of the disk should be the
     * volume ID.  However the practical reality is:
     *
     * (1) Only the first 20 characters are included by OpenStack.
     * (2) udev(?) adds extra stuff
     *
     * So look for any file under /dev/disk/by-id which contains
     * the prefix of the volume ID as a substring.
     *)
    let dev_disk_by_id =
      Option.default "/dev/disk/by-id" dev_disk_by_id in
    let prefix_len = 16 (* maybe 20, but be safe *) in
    let prefix_id =
      if String.length id > prefix_len then String.sub id 0 prefix_len
      else id in

    with_timeout ~sleep:5
      (sprintf (f_"waiting for cinder volume %s to attach to the conversion appliance") id)
      attach_timeout
      (fun () ->
        let entries =
          try Sys.readdir dev_disk_by_id
          (* It's possible for /dev/disk/by-id to not exist, since it's
           * only created by udev on demand, so ignore this error.
           *)
          with Sys_error _ -> [||] in
        let entries = Array.to_list entries in
        let entries =
          List.filter (fun e -> String.find e prefix_id >= 0) entries in
        match entries with
        | d :: _ -> Some (dev_disk_by_id // d)
        | [] -> None
      );
  in

  (* Attach volume IDs to the conversion appliance and wait
   * for the device nodes to appear.
   *)
  let devices =
    List.map (
      fun id ->
        let dev = attach_volume id in
        dev
    ) !volume_ids in

  (* Create nbdkit instances for each device node. *)
  List.iter (
    fun ((i, size), dev) ->
      let socket = sprintf "%s/out%d" dir i in
      cleanup_socket socket;

      output_local_file Sparse "raw" dev size socket
  ) (List.combine disks devices);

  (* Stash the volume IDs which will be needed again when finalizing. *)
  with_open_out (dir // "out.volume-ids")
    (fun chan -> output_value chan !volume_ids)

and openstack_finalize dir source inspect target_meta
                       (output_storage, server_id, guest_id, dev_disk_by_id,
                        run_openstack_command,
                        run_openstack_command_capture_json) =
  let volume_ids : string list =
    with_open_in (dir // "out.volume-ids") (fun chan -> input_value chan) in
  let nr_disks = List.length volume_ids in

  (* Update metadata on a cinder volume. *)
  let update_cinder_volume_metadata ?bootable ?description
                                    ?(image_properties = [])
                                    ?(volume_properties = [])
                                    id =
    let args = ref [ "volume"; "set" ] in

    Option.may (
      fun bootable ->
        List.push_back args
                       (if bootable then "--bootable" else "--non-bootable")
    ) bootable;

    Option.may (
      fun description ->
        List.push_back_list args ["--description"; description]
    ) description;

    let image_properties =
      List.flatten (
        List.map (
          fun (k, v) -> [ "--image-property"; sprintf "%s=%s" k v ]
        ) image_properties
      ) in
    List.push_back_list args image_properties;

    let volume_properties =
      List.flatten (
        List.map (
          fun (k, v) -> [ "--property"; sprintf "%s=%s" k v ]
        ) volume_properties
      ) in
    List.push_back_list args volume_properties;

    List.push_back args id;

    if run_openstack_command !args <> 0 then
      error (f_"openstack: failed to set image properties on cinder volume, see earlier error messages")
  in

  (* Image properties are only set on the first disk.
   *
   * In addition we set the first disk to bootable
   * (XXX see RHBZ#1308535 for why this is wrong).
   *)
  let image_properties =
    Openstack_image_properties.create source inspect target_meta in
  update_cinder_volume_metadata ~bootable:true ~image_properties
    (List.hd volume_ids);

  (* For all disks we update the description to a "non-temporary"
   * description (see above) and set volume properties.
   *)
  List.iteri (
    fun i id ->
      let description =
        sprintf "%s disk %d/%d converted by virt-v2v"
          target_meta.output_name (i+1) nr_disks in

      let volume_properties = ref [
        "virt_v2v_version", Config.package_version_full;
        "virt_v2v_conversion_date", iso_time;
        "virt_v2v_guest_name", target_meta.output_name;
        "virt_v2v_disk_index", sprintf "%d/%d" (i+1) nr_disks;
      ] in
      (match source.s_genid with
       | None -> ()
       | Some genid ->
          List.push_back volume_properties ("virt_v2v_vm_generation_id", genid)
      );
      (match guest_id with
       | None -> ()
       | Some guest_id ->
          List.push_back volume_properties ("virt_v2v_guest_id", guest_id)
      );
      let volume_properties = !volume_properties in

      update_cinder_volume_metadata ~description ~volume_properties id
  ) volume_ids

(* UTC conversion time. *)
and iso_time =
  let time = time () in
  let tm = gmtime time in
  sprintf "%04d/%02d/%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

(*----------------------------------------------------------------------*)
(* -om qemu *)

and qemu_print_output_options () =
  printf (f_"Output options (-oo) which can be used with -o qemu:

  -oo qemu-boot       Boot the guest in qemu after conversion
")

and qemu_parse_options cmdline =
  if cmdline.output_password <> None then
    error_option_cannot_be_used_in_output_mode "qemu" "-op";

  let qemu_boot = ref false in
  List.iter (
    fun (k, v) ->
      match k with
      | "qemu-boot" ->
         if v = "" || v = "true" then qemu_boot := true
         else if v = "false" then qemu_boot := false
         else
           error (f_"-o qemu: use -oo qemu-boot[=true|false]")
      | k ->
         error (f_"-o qemu: unknown output option ‘-oo %s’") k
    ) cmdline.output_options;
  let qemu_boot = !qemu_boot in

  (* -os must be set to a directory. *)
  let output_storage =
    match cmdline.output_storage with
    | None ->
       error (f_"-o qemu: output directory was not specified, use '-os /dir'")
    | Some d when not (is_directory d) ->
       error (f_"-os %s: output directory does not exist or is not a directory") d
    | Some d -> d in

  `Qemu (qemu_boot, cmdline.output_alloc, cmdline.output_format, output_storage)

and qemu_servers dir disks output_name
                 (_, output_alloc, output_format, output_storage) =
  List.iter (
    fun (i, size) ->
      let socket = sprintf "%s/out%d" dir i in
      cleanup_socket socket;

      (* Create the actual output disk. *)
      let outdisk = disk_path output_storage output_name i in
      output_local_file output_alloc output_format outdisk size socket
  ) disks

and qemu_finalize dir source inspect target_meta
                  (qemu_boot, output_alloc, output_format, output_storage) =
  let { guestcaps; output_name; target_buses; target_firmware } = target_meta in

  (* Convert metadata to libvirt XML. *)
  (match target_firmware with
   | TargetBIOS -> ()
   | TargetUEFI ->
      (* XXX Can remove this method when libvirt supports
       * <loader type="efi"/> since then it will be up to
       * libvirt to check this.
       *)
      error_unless_uefi_firmware guestcaps.gcaps_arch
  );

  let file = output_storage // output_name ^ ".sh" in

  let uefi_firmware =
    match target_firmware with
    | TargetBIOS -> None
    | TargetUEFI -> Some (find_uefi_firmware guestcaps.gcaps_arch) in
  let machine, secure_boot_required =
    match guestcaps.gcaps_machine, uefi_firmware with
    | _, Some { Uefi.flags }
         when List.mem Uefi.UEFI_FLAG_SECURE_BOOT_REQUIRED flags ->
       (* Force machine type to Q35 because PC does not support
        * secure boot.  We must remove this when we get the
        * correct machine type from libosinfo in future. XXX
        *)
       Q35, true
    | machine, _ ->
       machine, false in
  let smm = secure_boot_required in

  let machine =
    match machine with
    | I440FX -> "pc"
    | Q35 -> "q35"
    | Virt -> "virt" in

  (* Construct the command line.  Note that the [Qemuopts]
   * module deals with shell and qemu comma quoting.
   *)
  let cmd = Qemuopts.create () in
  Qemuopts.set_binary_by_arch cmd (Some guestcaps.gcaps_arch);

  let flag = Qemuopts.flag cmd
  and arg = Qemuopts.arg cmd
  and arg_noquote = Qemuopts.arg_noquote cmd
  and arg_list = Qemuopts.arg_list cmd in

  flag "-no-user-config"; flag "-nodefaults";
  arg "-name" output_name;

  (match source.s_genid with
   | None -> ()
   | Some genid ->
      arg_list "-device" ["vmgenid"; sprintf "guid=%s" genid; "id=vmgenid0"]
  );

  arg_list "-machine" (machine ::
                         (if smm then ["smm=on"] else []) @
                         ["accel=kvm:tcg"]);

  (match uefi_firmware with
   | None -> ()
   | Some { Uefi.code } ->
      if secure_boot_required then
        arg_list "-global"
          ["driver=cfi.pflash01"; "property=secure"; "value=on"];
      arg_list "-drive"
        ["if=pflash"; "format=raw"; "file=" ^ code; "readonly"];
      arg_noquote "-drive" "if=pflash,format=raw,file=\"$uefi_vars\"";
  );

  arg "-m" (Int64.to_string (source.s_memory /^ 1024L /^ 1024L));
  if source.s_vcpu > 1 then (
    (match source.s_cpu_topology with
     | None ->
        arg "-smp" (string_of_int source.s_vcpu)
     | Some { s_cpu_sockets; s_cpu_cores; s_cpu_threads } ->
        let args = [
            sprintf "cpus=%d" source.s_vcpu;
            sprintf "sockets=%d" s_cpu_sockets;
            sprintf "cores=%d" s_cpu_cores;
            sprintf "threads=%d" s_cpu_threads;
          ] in
        arg_list "-smp" args
    );
  );

  let make_disk if_name i = function
    | BusSlotEmpty -> ()

    | BusSlotDisk d ->
       (* Find the corresponding target disk. *)
       let outdisk = disk_path output_storage output_name d.s_disk_id in

       arg_list "-drive" ["file=" ^ outdisk; "format=" ^ output_format;
                          "if=" ^ if_name; "index=" ^ string_of_int i;
                          "media=disk"]

    | BusSlotRemovable { s_removable_type = CDROM } ->
       arg_list "-drive" ["format=raw"; "if=" ^ if_name;
                          "index=" ^ string_of_int i; "media=cdrom"]

    | BusSlotRemovable { s_removable_type = Floppy } ->
       arg_list "-drive" ["format=raw"; "if=" ^ if_name;
                          "index=" ^ string_of_int i; "media=floppy"]
  in
  Array.iteri (make_disk "virtio") target_buses.target_virtio_blk_bus;
  Array.iteri (make_disk "ide") target_buses.target_ide_bus;

  let make_scsi i = function
    | BusSlotEmpty -> ()

    | BusSlotDisk d ->
       (* Find the corresponding target disk. *)
       let outdisk = disk_path output_storage output_name d.s_disk_id in

       arg_list "-drive" ["file=" ^ outdisk; "format=" ^ output_format;
                          "if=scsi"; "bus=0"; "unit=" ^ string_of_int i;
                          "media=disk"]

    | BusSlotRemovable { s_removable_type = CDROM } ->
       arg_list "-drive" ["format=raw"; "if=scsi"; "bus=0";
                          "unit=" ^ string_of_int i; "media=cdrom"]

    | BusSlotRemovable { s_removable_type = Floppy } ->
       arg_list "-drive" ["format=raw"; "if=scsi"; "bus=0";
                          "unit=" ^ string_of_int i; "media=floppy"]
  in
  Array.iteri make_scsi target_buses.target_scsi_bus;

  (* XXX Highly unlikely that anyone cares, but the current
   * code ignores target_buses.target_floppy_bus.
   *)

  let net_bus =
    match guestcaps.gcaps_net_bus with
    | Virtio_net -> "virtio-net-pci"
    | E1000 -> "e1000"
    | RTL8139 -> "rtl8139" in
  List.iteri (
    fun i nic ->
      arg_list "-netdev" ["user"; "id=net" ^ string_of_int i];
      arg_list "-device" ([net_bus;
                           sprintf "netdev=net%d" i] @
                            (match nic.s_mac with
                             | None -> []
                             | Some mac -> ["mac=" ^ mac]))
    ) target_meta.target_nics;

  (* Add a display. *)
  (match source.s_display with
   | None -> ()
   | Some display ->
      (match display.s_display_type with
       | Window ->
          arg "-display" "gtk"
       | VNC ->
          arg "-display" "vnc=:0"
       | Spice ->
          arg_list "-spice" [sprintf "port=%d"
                               (match display.s_port with
                                | None -> 5900
                                | Some p -> p);
                             "addr=127.0.0.1"]
      );
      arg "-vga" "std"
  );

  (* Add a sound card. *)
  (match source.s_sound with
   | None -> ()
   | Some { s_sound_model = model } ->
      if qemu_supports_sound_card model then (
        match model with
        | AC97      -> arg "-device" "AC97"
        | ES1370    -> arg "-device" "ES1370"
        | ICH6      -> arg "-device" "intel-hda"; arg "-device" "hda-duplex"
        (* XXX ich9 is a q35-only device, so it's not likely
           that this will work unless we can force q35 above: *)
        | ICH9      -> arg "-device" "ich9-intel-hda"
        | PCSpeaker -> arg "-soundhw" "pcspk" (* not qdev-ified *)
        | SB16      -> arg "-device" "sb16"
        | USBAudio  -> arg "-device" "usb-audio"
      )
  );

  (* Add the miscellaneous KVM devices. *)
  if guestcaps.gcaps_virtio_rng then (
    arg_list "-object" ["rng-random"; "filename=/dev/urandom"; "id=rng0"];
    arg_list "-device" ["virtio-rng-pci"; "rng=rng0"];
  );
  if guestcaps.gcaps_virtio_balloon then
    arg "-balloon" "virtio"
  else
    arg "-balloon" "none";
  if guestcaps.gcaps_isa_pvpanic then
    arg_list "-device" ["pvpanic"; "ioport=0x505"];
  if guestcaps.gcaps_virtio_socket then
    arg "-viosock" "virtio"
  else
    arg "-viosock" "none";

  (* Add a serial console to Linux guests. *)
  if inspect.i_type = "linux" then
    arg "-serial" "stdio";

  (* Write the qemu script. *)
  with_open_out file (
    fun chan ->
      let fpf fs = fprintf chan fs in
      fpf "#!/bin/sh -\n";
      fpf "\n";

      (match uefi_firmware with
       | None -> ()
       | Some { Uefi.vars = vars_template } ->
          fpf "# Make a copy of the UEFI variables template\n";
          fpf "uefi_vars=\"$(mktemp)\"\n";
          fpf "cp %s \"$uefi_vars\"\n" (quote vars_template);
          fpf "\n"
      );

      Qemuopts.to_chan cmd chan
    );

  Unix.chmod file 0o755;

  (* If -oo qemu-boot option was specified then we should boot the guest. *)
  if qemu_boot then (
    let cmd = sprintf "%s &" (quote file) in
    ignore (shell_command cmd)
  )

(*----------------------------------------------------------------------*)
(* -om rhv-upload *)

and rhv_upload_print_output_options () =
  printf (f_"Output options (-oo) which can be used with -o rhv-upload:

  -oo rhv-cafile=CA.PEM           Set ‘ca.pem’ certificate bundle filename.
  -oo rhv-cluster=CLUSTERNAME     Set RHV cluster name.
  -oo rhv-direct[=true|false]     Use direct transfer mode (default: false).
  -oo rhv-verifypeer[=true|false] Verify server identity (default: false).

You can override the UUIDs of the disks, instead of using autogenerated UUIDs
after their uploads (if you do, you must supply one for each disk):

  -oo rhv-disk-uuid=UUID          Disk UUID
")

and rhv_upload_parse_options cmdline =
  let output_conn =
    match cmdline.output_conn with
    | None ->
       error (f_"-o rhv-upload: use ‘-oc’ to point to the oVirt or RHV server REST API URL, which is usually https://servername/ovirt-engine/api")
    | Some oc -> oc in
  (* In theory we could make the password optional in future. *)
  let output_password =
    match cmdline.output_password with
    | None ->
       error (f_"-o rhv-upload: output password file was not specified, use ‘-op’ to point to a file which contains the password used to connect to the oVirt or RHV server")
    | Some op -> op in
  let output_storage =
    match cmdline.output_storage with
    | None ->
       error (f_"-o rhv-upload: output storage was not specified, use ‘-os’");
    | Some os -> os in

  let rhv_cafile = ref None in
  let rhv_cluster = ref None in
  let rhv_direct = ref false in
  let rhv_verifypeer = ref false in
  let rhv_disk_uuids = ref None in

  List.iter (
    function
    | "rhv-cafile", v ->
       if !rhv_cafile <> None then
         error (f_"-o rhv-upload: -oo rhv-cafile set more than once");
       rhv_cafile := Some v
    | "rhv-cluster", v ->
       if !rhv_cluster <> None then
         error (f_"-o rhv-upload: -oo rhv-cluster set more than once");
       rhv_cluster := Some v
    | "rhv-direct", "" -> rhv_direct := true
    | "rhv-direct", v -> rhv_direct := bool_of_string v
    | "rhv-verifypeer", "" -> rhv_verifypeer := true
    | "rhv-verifypeer", v -> rhv_verifypeer := bool_of_string v
    | "rhv-disk-uuid", v ->
       if not (is_nonnil_uuid v) then
         error (f_"-o rhv-upload: invalid UUID for -oo rhv-disk-uuid");
       rhv_disk_uuids := Some (v :: (Option.default [] !rhv_disk_uuids))
    | k, _ ->
       error (f_"-o rhv-upload: unknown output option ‘-oo %s’") k
  ) cmdline.output_options;

  let rhv_cafile = !rhv_cafile in
  let rhv_cluster = !rhv_cluster in
  let rhv_direct = !rhv_direct in
  let rhv_verifypeer = !rhv_verifypeer in
  let rhv_disk_uuids = Option.map List.rev !rhv_disk_uuids in

  `RHVUpload (output_conn, cmdline.output_format,
              output_password, output_storage,
              rhv_cafile, rhv_cluster, rhv_direct,
              rhv_verifypeer, rhv_disk_uuids)

and rhv_upload_servers dir disks output_name
                       (output_conn, output_format,
                        output_password, output_storage,
                        rhv_cafile, rhv_cluster, rhv_direct,
                        rhv_verifypeer, rhv_disk_uuids) =
  (* We need nbdkit >= 1.22 for API_VERSION 2 and parallel threading model
   * in the python plugin.
   *)
  let nbdkit_min_version = (1, 22, 0) in
  let nbdkit_min_version_string = "1.22.0" in

  (* Check that the 'ovirtsdk4' Python module is available. *)
  let error_unless_ovirtsdk4_module_available () =
    let res = run_command [ Python_script.python; "-c"; "import ovirtsdk4" ] in
    if res <> 0 then
      error (f_"the Python module ‘ovirtsdk4’ could not be loaded, is it installed?  See previous messages for problems.")
  in

  (* Check that nbdkit is available and new enough. *)
  let error_unless_nbdkit_working () =
    if not (Nbdkit.is_installed ()) then
      error (f_"nbdkit is not installed or not working.  It is required to use ‘-o rhv-upload’.  See the virt-v2v-output-rhv(1) manual.")
  in

  let error_unless_nbdkit_min_version config =
    let version = Nbdkit.version config in
    if version < nbdkit_min_version then
      error (f_"nbdkit is not new enough, you need to upgrade to nbdkit ≥ %s")
        nbdkit_min_version_string
  in

  (* Check that the python3 plugin is installed and working
   * and can load the plugin script.
   *)
  let error_unless_nbdkit_python_plugin_working plugin_script =
    let cmd = sprintf "nbdkit python %s --dump-plugin >/dev/null"
                (quote (Python_script.path plugin_script)) in
    debug "%s" cmd;
    if Sys.command cmd <> 0 then
      error (f_"nbdkit python plugin is not installed or not working.  It is required if you want to use ‘-o rhv-upload’.

See also the virt-v2v-output-rhv(1) manual.");
  in

  (* Check that nbdkit was compiled with SELinux support (for the
   * --selinux-label option).
   *)
  let error_unless_nbdkit_compiled_with_selinux config =
    if have_selinux then (
      let selinux = try List.assoc "selinux" config with Not_found -> "no" in
      if selinux = "no" then
        error (f_"nbdkit was compiled without SELinux support.  You will have to recompile nbdkit with libselinux-devel installed, or else set SELinux to Permissive mode while doing the conversion.")
    )
  in

  Python_script.error_unless_python_interpreter_found ();
  error_unless_ovirtsdk4_module_available ();
  error_unless_nbdkit_working ();
  let config = Nbdkit.config () in
  error_unless_nbdkit_min_version config;
  error_unless_nbdkit_compiled_with_selinux config;

  (* Python code. *)
  let precheck_script =
    Python_script.create ~name:"rhv-upload-precheck.py"
      Output_rhv_upload_precheck_source.code in
  let vmcheck_script =
    Python_script.create ~name:"rhv-upload-vmcheck.py"
      Output_rhv_upload_vmcheck_source.code in
  let plugin_script =
    Python_script.create ~name:"rhv-upload-plugin.py"
      Output_rhv_upload_plugin_source.code in
  let transfer_script =
    Python_script.create ~name:"rhv-upload-transfer.py"
      Output_rhv_upload_transfer_source.code in
  let finalize_script =
    Python_script.create ~name:"rhv-upload-finalize.py"
      Output_rhv_upload_finalize_source.code in
  let cancel_script =
    Python_script.create ~name:"rhv-upload-cancel.py"
      Output_rhv_upload_cancel_source.code in
  let createvm_script =
    Python_script.create ~name:"rhv-upload-createvm.py"
      Output_rhv_upload_createvm_source.code in

  error_unless_nbdkit_python_plugin_working plugin_script;

  (* JSON parameters which are invariant between disks. *)
  let json_params = [
    "verbose", JSON.Bool (verbose ());

    "output_conn", JSON.String output_conn;
    "output_password", JSON.String output_password;
    "output_storage", JSON.String output_storage;
    "rhv_cafile", json_optstring rhv_cafile;
    "rhv_cluster",
      JSON.String (Option.default "Default" rhv_cluster);
    "rhv_direct", JSON.Bool rhv_direct;

    (* The 'Insecure' flag seems to be a number with various possible
     * meanings, however we just set it to True/False.
     *
     * https://github.com/oVirt/ovirt-engine-sdk/blob/19aa7070b80e60a4cfd910448287aecf9083acbe/sdk/lib/ovirtsdk4/__init__.py#L395
     *)
    "insecure", JSON.Bool (not rhv_verifypeer);
  ] in

  (* nbdkit command line which is invariant between disks. *)
  let nbdkit_cmd = Nbdkit.new_cmd in
  let nbdkit_cmd = Nbdkit.set_exportname nbdkit_cmd "/" in
  let nbdkit_cmd = Nbdkit.set_verbose nbdkit_cmd (verbose ()) in
  let nbdkit_cmd = Nbdkit.set_plugin nbdkit_cmd "python" in
  let nbdkit_cmd =
    Nbdkit.add_arg nbdkit_cmd "script" (Python_script.path plugin_script) in

  (* Match number of parallel coroutines in qemu-img *)
  let nbdkit_cmd = Nbdkit.set_threads nbdkit_cmd 8 in

  (* Python code prechecks. *)
  let json_params = match rhv_disk_uuids with
  | None -> json_params
  | Some uuids ->
     let ids = List.map (fun uuid -> JSON.String uuid) uuids in
     ("rhv_disk_uuids", JSON.List ids) :: json_params
  in
  let precheck_json = dir // "v2vprecheck.json" in
  let fd = Unix.openfile precheck_json [O_WRONLY; O_CREAT] 0o600 in
  if Python_script.run_command ~stdout_fd:fd
       precheck_script json_params [] <> 0 then
    error (f_"failed server prechecks, see earlier errors");
  let json = JSON_parser.json_parser_tree_parse_file precheck_json in
  debug "precheck output parsed as: %s"
    (JSON.string_of_doc ~fmt:JSON.Indented ["", json]);
  let rhv_storagedomain_uuid =
    Some (JSON_parser.object_get_string "rhv_storagedomain_uuid" json) in
  let rhv_cluster_uuid =
    Some (JSON_parser.object_get_string "rhv_cluster_uuid" json) in
  let rhv_cluster_cpu_architecture =
    Some (JSON_parser.object_get_string "rhv_cluster_cpu_architecture" json) in

  (* If the disk UUIDs were not provided, then generate them.
   * This is simpler than letting RHV generate them and trying
   * to read them back from RHV.
   *)
  let disk_uuids =
    match rhv_disk_uuids with
    | Some uuids ->
       let nr_disks = List.length disks in
       if List.length uuids <> nr_disks then
         error (f_"the number of ‘-oo rhv-disk-uuid’ parameters passed on th
e command line has to match the number of guest disk images (for this guest: %d)
") nr_disks;
       uuids
    | None -> List.map (fun _ -> uuidgen ()) disks in

  (* This will accumulate the list of transfer IDs from the transfer script. *)
  let transfer_ids = ref [] in

  let rhv_cluster_name =
    match List.assoc "rhv_cluster" json_params with
    | JSON.String s -> s
    | _ -> assert false in

  let json_params =
    ("output_name", JSON.String output_name) :: json_params in

  (* Check that the VM does not exist.  This can't run in #precheck because
   * we need to know the name of the virtual machine.
   *)
  if Python_script.run_command vmcheck_script json_params [] <> 0 then
    error (f_"failed vmchecks, see earlier errors");

  (* Cancel the transfer and delete disks.
   *
   * This ignores errors since the only time we are doing this is on
   * the failure path.
   *)
  let cancel transfer_ids disk_uuids =
    let ids = List.map (fun id -> JSON.String id) transfer_ids in
    let json_params = ("transfer_ids", JSON.List ids) :: json_params in
    let ids = List.map (fun uuid -> JSON.String uuid) disk_uuids in
    let json_params = ("disk_uuids", JSON.List ids) :: json_params in
    ignore (Python_script.run_command cancel_script json_params [])
  in

  (* Set up an at-exit handler so we delete the orphan disks on failure. *)
  at_exit (
    fun () ->
      (* virt-v2v writes v2vdir/done on success only. *)
      let success = Sys.file_exists (dir // "done") in
      if not success then (
        if disk_uuids <> [] then
          cancel !transfer_ids disk_uuids
      )
  );

  (* Create an nbdkit instance for each disk and set the
   * target URI to point to the NBD socket.
   *)
  List.iter (
    fun ((i, size), uuid) ->
      let socket = sprintf "%s/out%d" dir i in
      cleanup_socket socket;

      let disk_name = sprintf "%s-%03d" output_name i in
      let json_params =
        ("disk_name", JSON.String disk_name) :: json_params in

      let disk_format =
        match output_format with
        | "raw" as fmt -> fmt
        | "qcow2" as fmt -> fmt
        | _ ->
           error (f_"rhv-upload: -of %s: Only output format ‘raw’ or ‘qcow2’ is supported.  If the input is in a different format then force one of these output formats by adding either ‘-of raw’ or ‘-of qcow2’ on the command line.")
             output_format in
      let json_params =
        ("disk_format", JSON.String disk_format) :: json_params in

      let json_params =
        ("disk_size", JSON.Int size) :: json_params in

      let json_params =
        ("disk_uuid", JSON.String uuid) :: json_params in

      (* Write the JSON parameters to a file. *)
      let json_param_file = dir // sprintf "out.params%d.json" i in
      with_open_out
        json_param_file
        (fun chan -> output_string chan (JSON.string_of_doc json_params));

      (* Start the transfer. *)
      let transfer_json = dir // "v2vtransfer.json" in
      let fd = Unix.openfile transfer_json [O_WRONLY; O_CREAT] 0o600 in
      if Python_script.run_command ~stdout_fd:fd
           transfer_script json_params [] <> 0 then
        error (f_"failed to start transfer, see earlier errors");
      let json = JSON_parser.json_parser_tree_parse_file transfer_json in
      debug "transfer output parsed as: %s"
        (JSON.string_of_doc ~fmt:JSON.Indented ["", json]);
      let destination_url =
        JSON_parser.object_get_string "destination_url" json in
      let transfer_id =
        JSON_parser.object_get_string "transfer_id" json in
      List.push_back transfer_ids transfer_id;
      let is_ovirt_host =
        JSON_parser.object_get_bool "is_ovirt_host" json in

      (* Create the nbdkit instance. *)
      let cmd = nbdkit_cmd in
      let cmd = Nbdkit.add_arg cmd "size" (Int64.to_string size) in
      let cmd = Nbdkit.add_arg cmd "url" destination_url in
      let cmd =
        match rhv_cafile with
        | None -> cmd
        | Some cafile -> Nbdkit.add_arg cmd "cafile" cafile in
      let cmd =
        if not rhv_verifypeer then
          Nbdkit.add_arg cmd "insecure" "true"
        else cmd in
      let cmd =
        if is_ovirt_host then
          Nbdkit.add_arg cmd "is_ovirt_host" "true" else cmd in
      let _, pid = Nbdkit.run_unix ~socket cmd in

      (* --exit-with-parent should ensure nbdkit is cleaned
       * up when we exit, but it's not supported everywhere.
       *)
      cleanup_pid pid;
  ) (List.combine disks disk_uuids);

  (* Stash some data we will need during finalization. *)
  let disk_sizes = List.map snd disks in
  let t = (disk_sizes : int64 list), disk_uuids, !transfer_ids,
          finalize_script, createvm_script, json_params,
          rhv_storagedomain_uuid, rhv_cluster_uuid,
          rhv_cluster_cpu_architecture, rhv_cluster_name in
  with_open_out (dir // "out.rhv") (fun chan -> output_value chan t)

and rhv_upload_finalize dir source inspect target_meta
                        (output_conn, output_format,
                         output_password, output_storage,
                         rhv_cafile, rhv_cluster, rhv_direct,
                         rhv_verifypeer, rhv_disk_uuids) =
  let (disk_sizes : int64 list), disk_uuids, transfer_ids,
      finalize_script, createvm_script, json_params,
      rhv_storagedomain_uuid, rhv_cluster_uuid,
      rhv_cluster_cpu_architecture, rhv_cluster_name =
    with_open_in (dir // "out.rhv") (fun chan -> input_value chan) in

  (* Check the cluster CPU arch matches what we derived about the
   * guest during conversion.
   *)
  (match rhv_cluster_cpu_architecture with
   | None -> assert false
   | Some arch ->
      if arch <> target_meta.guestcaps.gcaps_arch then
        error (f_"the cluster ‘%s’ does not support the architecture %s but %s")
          rhv_cluster_name target_meta.guestcaps.gcaps_arch arch
  );

  (* Finalize all the transfers. *)
  let json_params =
    let ids = List.map (fun id -> JSON.String id) transfer_ids in
    let json_params = ("transfer_ids", JSON.List ids) :: json_params in
    let ids = List.map (fun uuid -> JSON.String uuid) disk_uuids in
    let json_params = ("disk_uuids", JSON.List ids) :: json_params in
    json_params in
  if Python_script.run_command finalize_script json_params [] <> 0 then
    error (f_"failed to finalize the transfers, see earlier errors");

  (* The storage domain UUID. *)
  let sd_uuid =
    match rhv_storagedomain_uuid with
    | None -> assert false
    | Some uuid -> uuid in

  (* The volume and VM UUIDs are made up. *)
  let vol_uuids = List.map (fun _ -> uuidgen ()) disk_sizes
  and vm_uuid = uuidgen () in

  (* Create the metadata. *)
  let ovf =
    Create_ovf.create_ovf source inspect target_meta disk_sizes
      Sparse output_format sd_uuid disk_uuids vol_uuids vm_uuid
      OVirt in
  let ovf = DOM.doc_to_string ovf in

  let json_params =
    match rhv_cluster_uuid with
    | None -> assert false
    | Some uuid -> ("rhv_cluster_uuid", JSON.String uuid) :: json_params in

  let ovf_file = dir // "vm.ovf" in
  with_open_out ovf_file (fun chan -> output_string chan ovf);
  if Python_script.run_command createvm_script json_params [ovf_file] <> 0
  then
    error (f_"failed to create virtual machine, see earlier errors")

and is_nonnil_uuid uuid =
  let nil_uuid = "00000000-0000-0000-0000-000000000000" in
  let rex_uuid = lazy (
    let hex = "[a-fA-F0-9]" in
    let str = sprintf "^%s{8}-%s{4}-%s{4}-%s{4}-%s{12}$" hex hex hex hex hex in
    PCRE.compile str
  ) in
  if uuid = nil_uuid then false
  else PCRE.matches (Lazy.force rex_uuid) uuid

and json_optstring = function
  | Some s -> JSON.String s
  | None -> JSON.Null

(*----------------------------------------------------------------------*)
(* -om rhv *)

and rhv_parse_options cmdline =
  if cmdline.output_password <> None then
    error_option_cannot_be_used_in_output_mode "rhv" "-op";

  (* -os must be set, but at this point we cannot check it. *)
  let output_storage =
    match cmdline.output_storage with
    | None -> error (f_"-o rhv: -os option was not specified")
    | Some d -> d in

  `RHV (cmdline.output_alloc, cmdline.output_format, output_storage)

and rhv_servers dir disks output_name
                (output_alloc, output_format, output_storage) =
  (* UID:GID required for files and directories when writing to ESD. *)
  let uid = 36 and gid = 36 in

  (* Create a UID-switching handle.  If we're not root, create a dummy
   * one because we cannot switch UIDs.
   *)
  let running_as_root = geteuid () = 0 in
  let changeuid_t =
    if running_as_root then
      Changeuid.create ~uid ~gid ()
    else
      Changeuid.create () in

  let esd_mp, esd_uuid =
    mount_and_check_storage_domain (s_"Export Storage Domain") output_storage in
  debug "RHV: ESD mountpoint: %s\nRHV: ESD UUID: %s" esd_mp esd_uuid;

  (* See if we can write files as UID:GID 36:36. *)
  let () =
    let testfile = esd_mp // esd_uuid // String.random8 () in
    Changeuid.make_file changeuid_t testfile "";
    let stat = stat testfile in
    Changeuid.unlink changeuid_t testfile;
    let actual_uid = stat.st_uid and actual_gid = stat.st_gid in
    debug "RHV: actual UID:GID of new files is %d:%d" actual_uid actual_gid;
    if uid <> actual_uid || gid <> actual_gid then (
      if running_as_root then
        warning (f_"cannot write files to the NFS server as %d:%d, even though we appear to be running as root. This probably means the NFS client or idmapd is not configured properly.\n\nYou will have to chown the files that virt-v2v creates after the run, otherwise RHV-M will not be able to import the VM.") uid gid
      else
        warning (f_"cannot write files to the NFS server as %d:%d. You might want to stop virt-v2v (^C) and rerun it as root.") uid gid
    ) in

  (* Create unique UUIDs for everything *)
  let vm_uuid = uuidgen () in
  (* Generate random image and volume UUIDs for each target disk. *)
  let image_uuids = List.map (fun _ -> uuidgen ()) disks in
  let vol_uuids = List.map (fun _ -> uuidgen ()) disks in

  (* We need to create the target image director(ies) so there's a place
   * for the main program to copy the images to.  However if image
   * conversion fails for any reason then we delete this directory.
   *)
  let images_dir = esd_mp // esd_uuid // "images" in
  List.iter (
    fun image_uuid ->
      let d = images_dir // image_uuid in
      Changeuid.mkdir changeuid_t d 0o755
  ) image_uuids;
  at_exit (
    fun () ->
      (* virt-v2v writes v2vdir/done on success only. *)
      let success = Sys.file_exists (dir // "done") in
      if not success then (
        List.iter (
          fun image_uuid ->
            let d = images_dir // image_uuid in
            let cmd = sprintf "rm -rf %s" (quote d) in
            Changeuid.command changeuid_t cmd
        ) image_uuids
      )
  );

  (* The final directory structure should look like this:
   *   /<MP>/<ESD_UUID>/images/
   *      <IMAGE_UUID_1>/<VOL_UUID_1>        # first disk
   *      <IMAGE_UUID_1>/<VOL_UUID_1>.meta   # first disk
   *      <IMAGE_UUID_2>/<VOL_UUID_2>        # second disk
   *      <IMAGE_UUID_2>/<VOL_UUID_2>.meta   # second disk
   *      <IMAGE_UUID_3>/<VOL_UUID_3>        # etc
   *      <IMAGE_UUID_3>/<VOL_UUID_3>.meta   #
   *)

  (* Generate the randomly named target files (just the names).
   * The main code is what generates the files themselves.
   *)
  let filenames =
    List.map (
      fun (image_uuid, vol_uuid) ->
        let filename = images_dir // image_uuid // vol_uuid in
        debug "RHV: disk: %s" filename;
        filename
    ) (List.combine image_uuids vol_uuids) in

  (* Generate the .meta file associated with each volume. *)
  let sizes = List.map snd disks in
  let metas =
    Create_ovf.create_meta_files output_alloc output_format
      esd_uuid image_uuids sizes in
  List.iter (
    fun (filename, meta) ->
      let meta_filename = filename ^ ".meta" in
      Changeuid.make_file changeuid_t meta_filename meta
  ) (List.combine filenames metas);

  (* Set up the NBD servers. *)
  List.iter (
    fun ((i, size), filename) ->
      let socket = sprintf "%s/out%d" dir i in
      cleanup_socket socket;

      (* Create the actual output disk. *)
      let changeuid f =
        Changeuid.func changeuid_t (fun () ->
          (* Run the command to create the file. *)
          f ();
          (* Make the file sufficiently writable so that possibly root, or
           * root squashed nbdkit will definitely be able to open it.
           * An example of how root squashing nonsense makes everyone
           * less secure.
           *)
          chmod filename 0o666
        )
      in
      output_local_file ~changeuid
                        output_alloc output_format filename size socket
  ) (List.combine disks filenames);

  (* Save parameters since we need them during finalization. *)
  let t = esd_mp, esd_uuid, vm_uuid, image_uuids, vol_uuids, sizes in
  with_open_out (dir // "out.esd") (fun chan -> output_value chan t)

and rhv_finalize dir source inspect target_meta
                 (output_alloc, output_format, output_storage) =
  (* Read back parameters saved during setup. *)
  let esd_mp, esd_uuid, vm_uuid, image_uuids, vol_uuids, sizes =
    with_open_in (dir // "out.esd") (fun chan -> input_value chan) in

  (* UID:GID required for files and directories when writing to ESD. *)
  let uid = 36 and gid = 36 in

  (* Create a UID-switching handle.  If we're not root, create a dummy
   * one because we cannot switch UIDs.
   *)
  let running_as_root = geteuid () = 0 in
  let changeuid_t =
    if running_as_root then
      Changeuid.create ~uid ~gid ()
    else
      Changeuid.create () in

  (* Create the metadata. *)
  let ovf =
    Create_ovf.create_ovf source inspect target_meta sizes
      output_alloc output_format esd_uuid image_uuids vol_uuids vm_uuid
      Create_ovf.RHVExportStorageDomain in

  (* Write it to the metadata file. *)
  let dir = esd_mp // esd_uuid // "master" // "vms" // vm_uuid in
  Changeuid.mkdir changeuid_t dir 0o755;
  let file = dir // vm_uuid ^ ".ovf" in
  Changeuid.output changeuid_t file (fun chan -> DOM.doc_to_chan chan ovf)

and mount_and_check_storage_domain domain_class os =
  (* The user can either specify -os nfs:/export, or a local directory
   * which is assumed to be the already-mounted NFS export.
   *)
  match String.split ":/" os with
  | mp, "" ->                         (* Already mounted directory. *)
    check_storage_domain domain_class os mp
  | server, export ->
    let export = "/" ^ export in

    (* Create a mountpoint.  Default mode is too restrictive for us
     * when we need to write into the directory as 36:36.
     *)
    let mp = Mkdtemp.temp_dir "v2v." in
    chmod mp 0o755;

    (* Try mounting it. *)
    let cmd = [ "mount"; sprintf "%s:%s" server export; mp ] in
    if run_command cmd <> 0 then
      error (f_"mount command failed, see earlier errors.\n\nThis probably means you didn't specify the right %s path [-os %s], or else you need to rerun virt-v2v as root.") domain_class os;

    (* Make sure it is unmounted at exit. *)
    at_exit (fun () ->
      let cmd = [ "umount"; mp ] in
      ignore (run_command cmd);
      try rmdir mp with _ -> ()
    );

    check_storage_domain domain_class os mp

and check_storage_domain domain_class os mp =
  (* Typical SD mountpoint looks like this:
   * $ ls /tmp/mnt
   * 39b6af0e-1d64-40c2-97e4-4f094f1919c7  __DIRECT_IO_TEST__  lost+found
   * $ ls /tmp/mnt/39b6af0e-1d64-40c2-97e4-4f094f1919c7
   * dom_md  images  master
   * We expect exactly one of those magic UUIDs.
   *)
  let entries =
    try Sys.readdir mp
    with Sys_error msg ->
      error (f_"could not read the %s specified by the '-os %s' parameter on the command line.  Is it really an OVirt or RHV-M %s?  The original error is: %s") domain_class os domain_class msg in
  let entries = Array.to_list entries in
  let uuids = List.filter (
    fun entry ->
      String.length entry = 36 &&
      entry.[8] = '-' && entry.[13] = '-' && entry.[18] = '-' &&
      entry.[23] = '-'
  ) entries in
  let uuid =
    match uuids with
    | [uuid] -> uuid
    | [] ->
      error (f_"there are no UUIDs in the %s (%s).  Is it really an OVirt or RHV-M %s?") domain_class os domain_class
    | _::_ ->
      error (f_"there are multiple UUIDs in the %s (%s).  This is unexpected, and may be a bug in virt-v2v or OVirt.") domain_class os in

  (* Check that the domain has been attached to a Data Center by
   * checking that the master/vms directory exists.
   *)
  let () =
    let master_vms_dir = mp // uuid // "master" // "vms" in
    if not (is_directory master_vms_dir) then
      error (f_"%s does not exist or is not a directory.\n\nMost likely cause: Either the %s (%s) has not been attached to any Data Center, or the path %s is not an %s at all.\n\nYou have to attach the %s to a Data Center using the RHV-M / OVirt user interface first.\n\nIf you don’t know what the %s mount point should be then you can also find this out through the RHV-M user interface.")
        master_vms_dir domain_class os os
        domain_class domain_class domain_class in

  (* Looks good, so return the SD mountpoint and UUID. *)
  (mp, uuid)

(*----------------------------------------------------------------------*)
(* -om vdsm *)

and vdsm_print_output_options () =
  let ovf_flavours_str = String.concat "|" Create_ovf.ovf_flavours in

  printf (f_"Output options (-oo) which can be used with -o vdsm:

  -oo vdsm-compat=0.10|1.1     Write qcow2 with compat=0.10|1.1
                                   (default: 0.10)
  -oo vdsm-vm-uuid=UUID        VM UUID (required)
  -oo vdsm-ovf-output=DIR      OVF metadata directory (required)
  -oo vdsm-ovf-flavour=%s
                               Set the type of generated OVF (default: rhvexp)

For each disk you must supply one of each of these options:

  -oo vdsm-image-uuid=UUID     Image directory UUID
  -oo vdsm-vol-uuid=UUID       Disk volume UUID
") ovf_flavours_str

and vdsm_parse_options cmdline =
  if cmdline.output_password <> None then
    error_option_cannot_be_used_in_output_mode "vdsm" "-op";

  let vm_uuid = ref None in
  let ovf_output = ref None in (* default "." *)
  let compat = ref "0.10" in
  let ovf_flavour = ref Create_ovf.RHVExportStorageDomain in
  let image_uuids = ref [] in
  let vol_uuids = ref [] in

  List.iter (
    function
    | "vdsm-compat", "0.10" -> compat := "0.10"
    | "vdsm-compat", "1.1" -> compat := "1.1"
    | "vdsm-compat", v ->
       error (f_"-o vdsm: unknown vdsm-compat level ‘%s’") v
    | "vdsm-vm-uuid", v ->
       if !vm_uuid <> None then
         error (f_"-o vdsm: -oo vdsm-vm-uuid set more than once");
       vm_uuid := Some v;
    | "vdsm-ovf-output", v ->
       if !ovf_output <> None then
         error (f_"-o vdsm: -oo vdsm-ovf-output set more than once");
       ovf_output := Some v;
    | "vdsm-ovf-flavour", v ->
       ovf_flavour := Create_ovf.ovf_flavour_of_string v
    | "vdsm-image-uuid", v ->
       List.push_front v image_uuids
    | "vdsm-vol-uuid", v ->
       List.push_front v vol_uuids
    | k, _ ->
       error (f_"-o vdsm: unknown output option ‘-oo %s’") k
  ) cmdline.output_options;

  let compat = !compat in
  let image_uuids = List.rev !image_uuids in
  let vol_uuids = List.rev !vol_uuids in
  if image_uuids = [] || vol_uuids = [] then
    error (f_"-o vdsm: either -oo vdsm-vol-uuid or -oo vdsm-vm-uuid was not specified");
  let vm_uuid =
    match !vm_uuid with
    | None ->
       error (f_"-o vdsm: -oo vdsm-image-uuid was not specified")
    | Some uuid -> uuid in
  let ovf_output = Option.default "." !ovf_output in
  let ovf_flavour = !ovf_flavour in

  (* -os must be set, but at this point we cannot check it. *)
  let output_storage =
    match cmdline.output_storage with
    | None -> error (f_"-o vdsm: -os option was not specified")
    | Some d when not (is_directory d) ->
       error (f_"-os %s: output directory does not exist or is not a directory") d
    | Some d -> d in

  `VDSM (cmdline.output_alloc, cmdline.output_format, output_storage,
         image_uuids, vol_uuids, vm_uuid, ovf_output,
         compat, ovf_flavour)

and vdsm_servers dir disks output_name
                 (output_alloc, output_format, output_storage,
                  image_uuids, vol_uuids, vm_uuid, ovf_output,
                  compat, ovf_flavour) =
  if List.length image_uuids <> List.length disks ||
     List.length vol_uuids <> List.length disks then
    error (f_"the number of ‘-oo vdsm-image-uuid’ and ‘-oo vdsm-vol-uuid’ parameters passed on the command line has to match the number of guest disk images (for this guest: %d)")
      (List.length disks);

  let dd_mp, dd_uuid =
    let fields =
      String.nsplit "/" output_storage in (* ... "data-center" "UUID" *)
    let fields = List.rev fields in       (* "UUID" "data-center" ... *)
    let fields = List.dropwhile ((=) "") fields in
    match fields with
    | uuid :: rest when String.length uuid = 36 ->
       let mp = String.concat "/" (List.rev rest) in
       mp, uuid
    | _ ->
       error (f_"vdsm: invalid -os parameter does not contain a valid UUID: %s")
         output_storage in

  debug "VDSM: DD mountpoint: %s\nVDSM: DD UUID: %s" dd_mp dd_uuid;

  (* Note that VDSM has to create all these directories. *)
  let images_dir = dd_mp // dd_uuid // "images" in
  List.iter (
    fun image_uuid ->
      let d = images_dir // image_uuid in
      if not (is_directory d) then
        error (f_"image directory (%s) does not exist or is not a directory")
          d
    ) image_uuids;

  (* Note that VDSM has to create this directory too. *)
  if not (is_directory ovf_output) then
    error (f_"OVF (metadata) directory (%s) does not exist or is not a directory")
      ovf_output;

  debug "VDSM: OVF (metadata) directory: %s" ovf_output;

  (* The final directory structure should look like this:
   *   /<MP>/<ESD_UUID>/images/
   *      <IMAGE_UUID_1>/<VOL_UUID_1>        # first disk
   *      <IMAGE_UUID_1>/<VOL_UUID_1>.meta   # first disk
   *      <IMAGE_UUID_2>/<VOL_UUID_2>        # second disk
   *      <IMAGE_UUID_2>/<VOL_UUID_2>.meta   # second disk
   *      <IMAGE_UUID_3>/<VOL_UUID_3>        # etc
   *      <IMAGE_UUID_3>/<VOL_UUID_3>.meta   #
   *)

  (* Create the target filenames. *)
  let filenames =
    List.map (
      fun (image_uuid, vol_uuid) ->
        let filename = images_dir // image_uuid // vol_uuid in
        debug "VDSM: disk: %s" filename;
        filename
    ) (List.combine image_uuids vol_uuids) in

  (* Generate the .meta files associated with each volume. *)
  let sizes = List.map snd disks in
  let metas =
    Create_ovf.create_meta_files output_alloc output_format
      dd_uuid image_uuids sizes in
  List.iter (
    fun (filename, meta) ->
      let meta_filename = filename ^ ".meta" in
      with_open_out meta_filename (fun chan -> output_string chan meta)
  ) (List.combine filenames metas);

  (* Set up the NBD servers. *)
  List.iter (
    fun ((i, size), filename) ->
      let socket = sprintf "%s/out%d" dir i in
      cleanup_socket socket;

      (* Create the actual output disk. *)
      output_local_file output_alloc output_format filename size socket
  ) (List.combine disks filenames);

  (* Save parameters since we need them during finalization. *)
  let t = dd_mp, dd_uuid, sizes in
  with_open_out (dir // "out.dd") (fun chan -> output_value chan t)

and vdsm_finalize dir source inspect target_meta
                  (output_alloc, output_format, output_storage,
                   image_uuids, vol_uuids, vm_uuid, ovf_output,
                   compat, ovf_flavour) =
  (* Read back parameters saved during setup. *)
  let dd_mp, dd_uuid, sizes =
    with_open_in (dir // "out.dd") (fun chan -> input_value chan) in

  (* Create the metadata. *)
  let ovf = Create_ovf.create_ovf source inspect target_meta sizes
              output_alloc output_format dd_uuid
              image_uuids
              vol_uuids
              vm_uuid
              ovf_flavour in

  (* Write it to the metadata file. *)
  let file = ovf_output // vm_uuid ^ ".ovf" in
  with_open_out file (fun chan -> DOM.doc_to_chan chan ovf)

let () = run_main_and_handle_errors main
