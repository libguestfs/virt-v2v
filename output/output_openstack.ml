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

open Printf
open Unix

open C_utils
open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Types
open Utils

open Output

module Openstack = struct
  type poptions = string option * string * string *
                  string option * string option *
                  (string list -> int) * (string list -> JSON.json_t option)

  type t = string list

  let to_string options =
    (* Try to get the server-id since it seems useful to display
     * that for diagnostics.
     *)
    let server_id = ref None in
    List.iter (
      function
      | "server-id", v -> server_id := Some v
      | _ -> ()
    ) options.output_options;

    "-o openstack" ^
      (match !server_id with
       | None -> ""
       | Some id -> sprintf " -oo server-id=%s" id)

  let query_output_options () =
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

  let parse_options options source =
    if options.output_alloc <> Sparse || options.output_format <> "raw" then
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
      | k, v when String.starts_with "os-" k ->
         (* Accumulate any remaining/unknown -oo os-* parameters
          * into the authentication list, where they will be
          * pass unmodified through to the openstack command.
          *)
         let opt = sprintf "--%s=%s" k v in
         authentication := opt :: !authentication
      | k, _ ->
         error (f_"-o openstack: unknown output option ‘-oo %s’") k
    ) options.output_options;
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
      Option.iter
        (fun oc -> List.push_back args (sprintf "--os-auth-url=%s" oc))
        options.output_conn;
      if not verify_server_certificate then
        List.push_back args "--insecure";
      !args in

    (* Check the openstack command exists. *)
    let error_unless_openstack_command_exists () =
      try ignore (which openstack_binary)
      with Executable_not_found _ ->
        error (f_"the ‘%s’ program is not available.  \
                  It is needed to communicate with OpenStack.")
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
      On_exit.unlink json;
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
      error (f_"openstack: precheck failed, there may be a problem with \
                authentication, see earlier error messages");

    let output_name = Option.value ~default:source.s_name options.output_name in

    (options.output_storage, output_name,
     server_id, guest_id, dev_disk_by_id,
     run_openstack_command,
     run_openstack_command_capture_json)

  let setup dir options source input_disks =
    let input_sizes = get_disk_sizes input_disks in
    let output_storage, output_name,
        server_id, guest_id, dev_disk_by_id,
        run_openstack_command, run_openstack_command_capture_json = options in

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
    On_exit.f (
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
                                 "--description"; description ];
      Option.iter (
        fun os -> List.push_back_list args [ "--type"; os ]
      ) output_storage;
      List.push_back args name;

      let json =
        match run_openstack_command_capture_json !args with
        | None ->
           error (f_"openstack: failed to create a cinder volume, \
                     see earlier error messages")
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
             error (f_"openstack: failed to query cinder volume status, \
                       see earlier error messages")
          | Some json ->
             match JSON_parser.object_get_string "status" json with
             | "creating" -> None
             | "available" -> Some () (* done *)
             | status ->
                error (f_"openstack: unknown volume status \"%s\": \
                          expected \"creating\" or \"available\"") status
      );

      id
    in

    (* Create the Cinder volumes. *)
    List.iteri (
      fun i size ->
        (* Unclear what we should set the name to, so just make
         * something related to the guest name.  Cinder volume
         * names do not need to be unique.
         *)
        let name = disk_name output_name i in

        (* Create the cinder volume. *)
        let id = create_cinder_volume name description size in
        List.push_back volume_ids id
    ) input_sizes;

    (* Attach volume to current VM and wait for it to appear.
     * Returns the block device name.
     *)
    let attach_volume id =
      let args = [ "server"; "add"; "volume"; server_id; id ] in
      if run_openstack_command args <> 0 then
        error (f_"openstack: failed to attach cinder volume to VM, \
                  see earlier error messages");

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
        Option.value ~default:"/dev/disk/by-id" dev_disk_by_id in
      let prefix_len = 16 (* maybe 20, but be safe *) in
      let prefix_id =
        if String.length id > prefix_len then String.sub id 0 prefix_len
        else id in

      with_timeout ~sleep:5
        (sprintf (f_"waiting for cinder volume %s to attach to the \
                     conversion appliance") id)
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
    let devices = List.map attach_volume !volume_ids in

    (* Create nbdkit instances for each device node. *)
    let uris =
      List.mapi (
        fun i (size, dev) ->
          let socket = sprintf "%s/out%d" dir i in
          On_exit.unlink socket;

          output_to_local_file Sparse "raw" dev size socket;

          NBD_URI.Unix (socket, None)
      ) (List.combine input_sizes devices) in

    !volume_ids, uris

  let rec finalize dir options volume_ids output_disks
            source inspect target_meta =
    let output_storage, output_name,
        server_id, guest_id, dev_disk_by_id,
        run_openstack_command, run_openstack_command_capture_json = options in
    let nr_disks = List.length volume_ids in

    (* Update metadata on a cinder volume. *)
    let update_cinder_volume_metadata ?bootable ?description
                                      ?(image_properties = [])
                                      ?(volume_properties = [])
                                      id =
      let args = ref [ "volume"; "set" ] in

      Option.iter (
        fun bootable ->
          List.push_back args
            (if bootable then "--bootable" else "--non-bootable")
      ) bootable;

      Option.iter (
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
        error (f_"openstack: failed to set image properties on cinder volume, \
                  see earlier error messages")
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
            output_name (i+1) nr_disks in

        let volume_properties = ref [
          "virt_v2v_version", Config.package_version_full;
          "virt_v2v_conversion_date", iso_time;
          "virt_v2v_guest_name", output_name;
          "virt_v2v_disk_index", sprintf "%d/%d" (i+1) nr_disks;
        ] in
        (match source.s_genid with
         | None -> ()
         | Some genid ->
            List.push_back volume_properties
              ("virt_v2v_vm_generation_id", genid)
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

  let request_size = None
end
