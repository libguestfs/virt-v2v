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

(* Utilities used in virt-v2v only. *)

open Printf

open Std_utils
open Tools_utils
open Unix_utils
open Common_gettext.Gettext

let large_tmpdir =
  try Sys.getenv "VIRT_V2V_TMPDIR"
  with Not_found -> (open_guestfs ())#get_cachedir ()

let string_of_process_status = function
  | Unix.WEXITED 0 -> s_"success"
  | WEXITED i -> sprintf (f_"exited with non-zero error code %d") i
  | WSIGNALED i -> sprintf (f_"signalled by signal %d") i
  | WSTOPPED i -> sprintf (f_"stopped by signal %d") i

(* Is SELinux enabled and enforcing on the host? *)
let have_selinux =
  let cmd = "getenforce 2>/dev/null | grep -isq Enforcing" in
  let test = lazy (0 = Sys.command cmd) in
  fun () -> Lazy.force test

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

(* Escape characters like [?] and [*] which are special for fnmatch(3). *)
let fnmatch_escape str =
  let len = String.length str in
  let xs = ref [] in
  for i = 0 to len-1 do
    xs :=
      (match str.[i] with
       | '[' | ']' | '?' | '*' as c ->
          sprintf "\\%c" c
       | c ->
          String.make 1 c
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
    let user = Option.value ~default:"qemu" (libvirt_qemu_user ()) in
    let uid =
      if String.starts_with "+" user then
        int_of_string (String.sub user 1 (String.length user - 1))
      else
        (Unix.getpwnam user).pw_uid in
    debug "setting owner of %s to %d:root" file uid;
    Unix.chown file uid 0
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
    error (f_"ssh-agent authentication has not been set up ($SSH_AUTH_SOCK \
              is not set).  This is required by qemu to do passwordless \
              ssh access.  See the virt-v2v(1) man page for more information.")

let nbdcopy_supports_blkhash =
  let check =
    lazy (
      let cmd = sprintf "%s --help | grep -sq -- --blkhash" Config.nbdcopy in
      0 = Sys.command cmd
    ) in
  fun () -> Lazy.force check

(* Create the directory containing inX and outX sockets. *)
let create_v2v_directory () =
  let d = Mkdtemp.temp_dir "v2v." in
  On_exit.rm_rf d;
  chown_for_libvirt_rhbz_1045069 d;
  d

(* Wait for a file to appear until a timeout. *)
let rec wait_for_file filename timeout =
  if Sys.file_exists filename then true
  else if timeout = 0 then false
  else (
    Unix.sleep 1;
    wait_for_file filename (timeout-1)
  )

let with_nbd_connect_uri ?(meta_contexts = []) ~uri f =
  let nbd = NBD.create () in
  Fun.protect
    (fun () ->
          NBD.set_debug nbd (verbose ());
          List.iter (NBD.add_meta_context nbd) meta_contexts;
          NBD.connect_uri nbd uri;
          Fun.protect
            (fun () -> f nbd)
            ~finally:(fun () -> NBD.shutdown nbd)
       )
    ~finally:(fun () -> NBD.close nbd)

let get_disk_allocated uri =
  let alloc_ctx = "base:allocation" in
  with_nbd_connect_uri ~uri ~meta_contexts:[alloc_ctx]
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

let get_uefi_arch_suffix = function
  | "x86_64" -> Some "X64"
  | "i386" -> Some "X32"
  | _ -> None

let name_from_disk disk =
  let name = Filename.basename disk in
  (* Remove the extension (or suffix), only if it's one usually
   * used for disk images. *)
  let suffixes = [
    ".img"; ".ova"; ".qcow2"; ".raw"; ".vmdk"; ".vmx";
    "-sda";
  ] in
  let rec loop = function
    | suff :: xs ->
       if Filename.check_suffix name suff then
         Filename.chop_suffix name suff
       else
         loop xs
    | [] -> name
  in
  let name = loop suffixes in
  if name = "" then
    error (f_"invalid input filename (%s)") disk;
  name
