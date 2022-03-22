(* virt-v2v
 * Copyright (C) 2009-2020 Red Hat Inc.
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

(* Utilities used in virt-v2v only. *)

open Printf

open Std_utils
open Tools_utils
open Unix_utils
open Common_gettext.Gettext

let large_tmpdir =
  try Sys.getenv "VIRT_V2V_TMPDIR"
  with Not_found -> (open_guestfs ())#get_cachedir ()

(* Is SELinux enabled and enforcing on the host? *)
let have_selinux =
  0 = Sys.command "getenforce 2>/dev/null | grep -isq Enforcing"

(* URI quoting. *)
let uri_quote str =
  let len = String.length str in
  let xs = ref [] in
  for i = 0 to len-1 do
    xs :=
      (match str.[i] with
      | ('A'..'Z' | 'a'..'z' | '0'..'9' | '/' | '.' | '-') as c ->
        String.make 1 c
      | c ->
        sprintf "%%%02x" (Char.code c)
      ) :: !xs
  done;
  String.concat "" (List.rev !xs)

(* Map guest architecture found by inspection to the architecture
 * that KVM must emulate.  Note for x86 we assume a 64 bit hypervisor.
 *)
let kvm_arch = function
  | "i386" | "i486" | "i586" | "i686"
  | "x86_64" -> "x86_64"
  | "unknown" -> "x86_64" (* most likely *)
  | arch -> arch

(* Does qemu support the given sound card? *)
let qemu_supports_sound_card = function
  | Types.AC97
  | Types.ICH6
  | Types.ICH9
  | Types.PCSpeaker
    -> true
  | Types.ES1370
  | Types.SB16
  | Types.USBAudio
    -> false

(* Find the UEFI firmware. *)
let find_uefi_firmware guest_arch =
  let files =
    (* The lists of firmware are actually defined in common/utils/uefi.c. *)
    match guest_arch with
    | "x86_64" -> Uefi.uefi_x86_64_firmware
    | "aarch64" -> Uefi.uefi_aarch64_firmware
    | arch ->
       error (f_"don’t know how to convert UEFI guests for architecture %s")
             guest_arch in
  let rec loop = function
    | [] ->
       error (f_"cannot find firmware for UEFI guests.\n\nYou probably need to install OVMF (x86-64), or AAVMF (aarch64)")
    | ({ Uefi.code; vars = vars_template } as ret) :: rest ->
       if Sys.file_exists code && Sys.file_exists vars_template then ret
       else loop rest
  in
  loop files

let error_unless_uefi_firmware guest_arch =
  ignore (find_uefi_firmware guest_arch)

let compare_app2_versions app1 app2 =
  let i = compare app1.Guestfs.app2_epoch app2.Guestfs.app2_epoch in
  if i <> 0 then i
  else (
    let i =
      compare_version app1.Guestfs.app2_version app2.Guestfs.app2_version in
    if i <> 0 then i
    else
      compare_version app1.Guestfs.app2_release app2.Guestfs.app2_release
  )

let du filename =
  (* There's no OCaml binding for st_blocks, so run coreutils 'du'. *)
  let cmd =
    sprintf "du --block-size=1 %s | awk '{print $1}'" (quote filename) in
  (* XXX This can call error and so exit, but it would be preferable
   * to raise an exception here.
   *)
  let lines = external_command cmd in
  match lines with
  | line::_ -> Int64.of_string line
  | [] -> invalid_arg filename

let qemu_img_supports_offset_and_size () =
  (* We actually attempt to create a qcow2 file with a raw backing
   * file that has an offset and size.
   *)
  let tmp = Filename.temp_file "v2vqemuimgtst" ".img" in
  On_exit.unlink tmp;
  Unix.truncate tmp 1024;

  let json = [
      "file", JSON.Dict [
        "driver", JSON.String "raw";
        "offset", JSON.Int 512_L;
        "size", JSON.Int 512_L;
        "file", JSON.Dict [
          "filename", JSON.String tmp
        ]
      ]
  ] in

  let cmd =
    sprintf "qemu-img info json:%s >/dev/null%s"
            (quote (JSON.string_of_doc ~fmt:JSON.Compact json))
            (if verbose () then "" else " 2>&1") in
  debug "%s" cmd;
  let r = 0 = Sys.command cmd in
  debug "qemu-img supports \"offset\" and \"size\" in json URLs: %b" r;
  r

let backend_is_libvirt () =
  let backend = (open_guestfs ())#get_backend () in
  let backend = fst (String.split ":" backend) in
  backend = "libvirt"

let rec chown_for_libvirt_rhbz_1045069 file =
  let running_as_root = Unix.geteuid () = 0 in
  if running_as_root && backend_is_libvirt () then (
    try
      let user = Option.default "qemu" (libvirt_qemu_user ()) in
      let uid =
        if String.is_prefix user "+" then
          int_of_string (String.sub user 1 (String.length user - 1))
        else
          (Unix.getpwnam user).pw_uid in
      debug "setting owner of %s to %d:root" file uid;
      Unix.chown file uid 0
    with
    | exn -> (* Print exception, but continue. *)
       debug "could not set owner of %s: %s"
         file (Printexc.to_string exn)
  )

(* Get the local user that libvirt uses to run qemu when we are
 * running as root.  This is returned as an optional string
 * containing the username.  The username might be "+NNN"
 * meaning a numeric UID.
 * https://listman.redhat.com/archives/libguestfs/2022-March/028450.html
 *)
and libvirt_qemu_user =
  let user =
    lazy (
      let conn = Libvirt.Connect.connect_readonly () in
      let xml = Libvirt.Connect.get_capabilities conn in
      let doc = Xml.parse_memory xml in
      let xpathctx = Xml.xpath_new_context doc in
      let expr =
        "//secmodel[./model=\"dac\"]/baselabel[@type=\"kvm\"]/text()" in
      let uid_gid = Xpath_helpers.xpath_string xpathctx expr in
      match uid_gid with
      | None -> None
      | Some uid_gid ->
         (* The string will be something like "+107:+107", return the
          * UID part.
          *)
         Some (fst (String.split ":" uid_gid))
    ) in
  fun () -> Lazy.force user

(* When using the SSH driver in qemu (currently) this requires
 * ssh-agent authentication.  Give a clear error if this hasn't been
 * set up (RHBZ#1139973).  This might improve if we switch to libssh1.
 *)
let error_if_no_ssh_agent () =
  try ignore (Sys.getenv "SSH_AUTH_SOCK")
  with Not_found ->
    error (f_"ssh-agent authentication has not been set up ($SSH_AUTH_SOCK is not set).  This is required by qemu to do passwordless ssh access.  See the virt-v2v(1) man page for more information.")

(* Create the directory containing inX and outX sockets. *)
let create_v2v_directory () =
  let d = Mkdtemp.temp_dir "v2v." in
  chown_for_libvirt_rhbz_1045069 d;
  On_exit.rmdir d;
  d

(* Wait for a file to appear until a timeout. *)
let rec wait_for_file filename timeout =
  if Sys.file_exists filename then true
  else if timeout = 0 then false
  else (
    Unix.sleep 1;
    wait_for_file filename (timeout-1)
  )

let with_nbd_connect_unix ?(meta_contexts = []) ~socket f =
  let nbd = NBD.create () in
  protect
    ~f:(fun () ->
          NBD.set_debug nbd (verbose ());
          List.iter (NBD.add_meta_context nbd) meta_contexts;
          NBD.connect_unix nbd socket;
          protect
            ~f:(fun () -> f nbd)
            ~finally:(fun () -> NBD.shutdown nbd)
       )
    ~finally:(fun () -> NBD.close nbd)

let get_disk_allocated ~dir ~disknr =
  let socket = sprintf "%s/out%d" dir disknr
  and alloc_ctx = "base:allocation" in
  with_nbd_connect_unix ~socket ~meta_contexts:[alloc_ctx]
    (fun nbd ->
         if NBD.can_meta_context nbd alloc_ctx then (
           (* Get the list of extents, using a 2GiB chunk size as hint. *)
           let size = NBD.get_size nbd
           and allocated = ref 0_L
           and fetch_offset = ref 0_L in
           while !fetch_offset < size do
             let remaining = size -^ !fetch_offset in
             let fetch_size = min 0x8000_0000_L remaining in
             NBD.block_status nbd fetch_size !fetch_offset
               (fun ctx offset entries err ->
                  assert (ctx = alloc_ctx);
                  for i = 0 to Array.length entries / 2 - 1 do
                    let len = entries.(i * 2)
                    and typ = entries.(i * 2 + 1) in
                    assert (len > 0_L);
                    if typ &^ 1_L = 0_L then
                      allocated := !allocated +^ len;
                    fetch_offset := !fetch_offset +^ len
                  done;
                  0
               )
           done;
           Some !allocated
         ) else None
       )
