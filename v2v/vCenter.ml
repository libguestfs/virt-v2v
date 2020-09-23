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

open Printf

open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Xml
open Utils

let source_re = PCRE.compile "^\\[(.*)\\] (.*)\\.vmdk$"
let snapshot_re = PCRE.compile "^(.*)-\\d{6}(\\.vmdk)$"

let rec qemu_uri_of_path ?bandwidth ?password_file dcPath uri server path =
  (* If no_verify=1 was passed in the libvirt URI, then we have to
   * turn off certificate verification here too.
   *)
  let sslverify =
    match uri.uri_query_raw with
    | None -> true
    | Some query ->
       (* XXX only works if the query string is not URI-quoted *)
       String.find query "no_verify=1" = -1 in

  (* Check the URL exists and authentication info is correct. *)
  let https_url =
    let https_url = get_https_url dcPath uri server path in
    let status, dump_response =
      fetch_headers_from_url password_file uri sslverify https_url in

    (* If a disk is actually a snapshot image it will have '-00000n'
     * appended to its name, e.g.:
     *   [yellow:storage1] RHEL4-X/RHEL4-X-000003.vmdk
     * The flat storage is still called RHEL4-X-flat, however. If we got
     * a 404 and the vmdk name looks like it might be a snapshot, try
     * again without the snapshot suffix.
     *)
    let https_url, status, dump_response =
      if status = "404" && PCRE.matches snapshot_re path then (
        let path = PCRE.sub 1 ^ PCRE.sub 2 in
        let https_url = get_https_url dcPath uri server path in
        let status, dump_response =
          fetch_headers_from_url password_file uri sslverify https_url in
        https_url, status, dump_response
      )
      else (https_url, status, dump_response) in

    if status = "401" then (
      dump_response stderr;
      if uri.uri_user <> None then
        error (f_"vcenter: incorrect username or password")
      else
        error (f_"vcenter: incorrect username or password.  You might need to specify the username in the URI like this: [vpx|esx|..]://USERNAME@[etc]")
    );

    if status = "404" then (
      dump_response stderr;
      error (f_"vcenter: URL not found: %s") https_url
    );

    if status <> "200" then (
      dump_response stderr;
      error (f_"vcenter: invalid response from server: %s") status
    );

    https_url in

  (* Write a cookie script to retrieve the session cookie.
   * See nbdkit-curl-plugin(1) "Example: VMware ESXi cookies"
   *)
  let cookie_script, chan =
    Filename.open_temp_file ~perms:0o700 "v2vcs" ".sh" in
  unlink_on_exit cookie_script;
  let fpf fs = fprintf chan fs in
  fpf "#!/bin/sh -\n";
  fpf "\n";
  fpf "curl --head -s";
  if not sslverify then fpf " --insecure";
  (match uri.uri_user, password_file with
   | None, None -> ()
   | Some user, None -> fpf " -u %s" (quote user)
   | None, Some password_file ->
      fpf " -u \"$LOGNAME\":\"$(cat %s)\"" (quote password_file)
   | Some user, Some password_file ->
      fpf " -u %s:\"$(cat %s)\"" (quote user) (quote password_file)
  );
  fpf " %s" (quote https_url);
  fpf " |\n";
  fpf "\tsed -ne %s\n" (quote "{ s/^Set-Cookie: \\([^;]*\\);.*/\\1/ip }");
  close_out chan;

  (* VMware authentication expires after 30 minutes so we must renew
   * after < 30 minutes.
   *)
  let cookie_script_renew = 25*60 in

  let nbdkit =
    Nbdkit_sources.create_curl ?bandwidth ~cookie_script ~cookie_script_renew
                               ~sslverify https_url in
  let qemu_uri = Nbdkit_sources.run nbdkit in

  (* Return the QEMU URI. *)
  qemu_uri

and get_https_url dcPath uri server path =
  if not (PCRE.matches source_re path) then
    path
  else (
    let datastore = PCRE.sub 1 and path = PCRE.sub 2 in

    let port =
      match uri.uri_port with
      | 443 -> ""
      | n when n >= 1 -> ":" ^ string_of_int n
      | _ -> "" in

    (* XXX Need to handle templates.  The file is called "-delta.vmdk" in
     * place of "-flat.vmdk".
     *)
    sprintf "https://%s%s/folder/%s-flat.vmdk?dcPath=%s&dsName=%s"
            server port
            (uri_quote path) (uri_quote dcPath) (uri_quote datastore)
  )

(* Fetch the status from a URL. *)
and fetch_headers_from_url password_file uri sslverify https_url =
  let curl_args = ref [
    "head", None;
    "silent", None;
    "url", Some https_url;
  ] in
  (match uri.uri_user, password_file with
   | None, None -> ()
   | None, Some _ ->
      warning (f_"-ip PASSWORD_FILE parameter ignored because 'user@' was not given in the URL")
   | Some user, None ->
      List.push_back curl_args ("user", Some user)
   | Some user, Some password_file ->
      let password = read_first_line_from_file password_file in
      List.push_back curl_args ("user", Some (user ^ ":" ^ password))
  );
  if not sslverify then List.push_back curl_args ("insecure", None);

  let curl_h = Curl.create !curl_args in
  let lines = Curl.run curl_h in

  let dump_response chan =
    Curl.print chan curl_h;

    (* Dump out the output of the command. *)
    List.iter (fun x -> fprintf chan "%s\n" x) lines;
    flush chan
  in

  if verbose () then dump_response stderr;

  let statuses, headers =
    List.partition (
      fun line ->
        let len = String.length line in
        len >= 12 && String.sub line 0 5 = "HTTP/"
    ) lines in

  (* Look for the last HTTP/x.y NNN status code in the output. *)
  let status =
    match statuses with
    | [] ->
       dump_response stderr;
       error (f_"vcenter: no status code in output of ‘curl’ command.")
    | ss ->
      let s = List.hd (List.rev ss) in
      String.sub s (String.index s ' ' + 1) 3 in

  status, dump_response
