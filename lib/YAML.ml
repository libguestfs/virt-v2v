(* virt-v2v
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

open Std_utils
open Tools_utils
open Common_gettext.Gettext

open Printf

let spaces = String.spaces

type node =
  | Assoc of (string * node) list
  | List of node list
  | String of string
  | Int of int
  | Bool of bool
  | Float of float
  | Block of string list
type doc = Doc of node

(* It's extremely difficult to give a complete list of strings
 * that have to be quoted, versus strings that can be safely
 * used as barewords.  For example strings like "Yes", "no",
 * "true", "123", "123.0" cannot be written as barewords since
 * they will be misinterpreted as booleans, integers or floats.
 * (These rules also depend on the version of YAML!)
 *
 * See also YAML 1.2 section 2.4 "Tags".
 *
 * In the regular expressions we're just trying to match things
 * which might be misinterpreted so that we know when to quote strings.
 * It doesn't matter if we match too eagerly here.
 *)
let re_reserved = PCRE.compile "^[\\[\\-\\]{}>|*&!%#'@,?:]"
let re_numeric1 = PCRE.compile ~caseless:true "^[-+.0-9exo]+$"
let re_numeric2 = PCRE.compile ~caseless:true "(inf|nan)$"
let re_boolean = PCRE.compile ~caseless:true "^(y|yes|n|no|true|false|on|off)$"

let needs_quoting str =
  String.length str = 0 ||
  PCRE.matches re_reserved str ||
  PCRE.matches re_numeric1 str ||
  PCRE.matches re_numeric2 str ||
  PCRE.matches re_boolean str ||
  let n = String.length str in
  let rec loop i =
    if i >= n then false
    else if String.unsafe_get str i <= ' ' then true
    else loop (i+1)
  in
  loop 0

let rec node_to_buf buf indent = function
  | Assoc xs -> assoc_to_buf buf indent xs
  | List xs -> list_to_buf buf indent xs
  | String str -> string_to_buf buf str
  | Int i -> int_to_buf buf i
  | Bool b -> bool_to_buf buf b
  | Float f -> float_to_buf buf f
  | Block lines -> block_to_buf buf indent lines

and assoc_to_buf buf ?(nl = true) indent =
  function
  (* Special case empty dictionary: https://stackoverflow.com/a/33510095 *)
  | [] -> bprintf buf "{}"
  | xs ->
    let nl = ref nl in
    List.iter (
      fun (key, value) ->
        (* Print newline and indent, if nl is true. *)
        if !nl then bprintf buf "\n%s" (spaces indent);
        nl := true;

        (* If the key is "#" then it is printed as a comment. *)
        if key = "#" then (
          match value with
          | String comment -> bprintf buf "# %s" comment
          | _ -> assert false
        )
        else (
          bprintf buf "%s:" key;
          (* Do we need a space after the key?  This is basically
           * about whether node_to_buf will print \n, which breaks
           * encapsulation of this function.  Make a best guess.
           *)
          (match value with
           | String _ | Int _ | Bool _ | Float _ | Block _
           | Assoc [] | List [] ->
              bprintf buf " "
           | _ -> ()
          );
          (* Within Assoc, Lists are not indented, everything else is. *)
          let indent_by = function List _ -> 0 | _ -> 2 in
          node_to_buf buf (indent + indent_by value) value
        )
    ) xs

and list_to_buf buf indent =
  function
  (* Special case empty list, I think is required.  We could also special
   * case other things, eg. lists of short, unquoted strings, but
   * let's not overcomplicate things.
   *)
  | [] -> bprintf buf "[]"
  | xs ->
    List.iter (
      fun node ->
        bprintf buf "\n%s- " (spaces indent);
        (* If it's an assoc list, then don't print first newline. *)
        match node with
        | Assoc xs -> assoc_to_buf buf ~nl:false (indent+2) xs
        | _ -> node_to_buf buf (indent+2) node
    ) xs

(* Strings are always printed on the same line. *)
and string_to_buf buf str =
  if not (needs_quoting str) then bprintf buf "%s" str
  else c_quoted_string_to_buf buf str

and c_quoted_string_to_buf buf str =
  bprintf buf "\"";
  for i = 0 to String.length str - 1 do
    let c = String.unsafe_get str i in
    if c = '"' then bprintf buf "\\\""
    else if c < ' ' then bprintf buf "\\x%02x" (Char.code c)
    else bprintf buf "%c" c
  done;
  bprintf buf "\""

and int_to_buf buf i = bprintf buf "%d" i

and bool_to_buf buf b = bprintf buf "%b" b

and float_to_buf buf f = bprintf buf "%f" f

(* Blocks are printed using "|" + "\n" followed by the indented lines
 * of text.  We implicitly assume that each line is followed by \n
 * so we don't have to use a chomp mode, but this page is interesting
 * in case we need to in future: https://yaml-multiline.info/
 *)
and block_to_buf buf indent lines =
  (* If a line contains \n within the line then that indicates
   * a bug in virt-v2v.  Since this could be used to bypass
   * indenting -- thus generating arbitrary YAML -- detect this
   * situation and error out.  As far as I'm aware, no other
   * character in blocks needs to be handled specially.
   *)
  List.iter (
    fun line ->
      if String.contains line '\n' || String.contains line '\r' then
        error (f_"YAML block contains newline character.  \
                  This should not happen, please report a bug against \
                  virt-v2v.")
  ) lines;

  bprintf buf "|";
  let indent = indent+2 in
  List.iter (
    fun line -> bprintf buf "\n%s%s" (spaces indent) line
  ) lines

let doc_to_buf buf (Doc node) =
  bprintf buf "---";
  node_to_buf buf 0 node;
  bprintf buf "\n"

let doc_to_string doc =
  let buf = Buffer.create 4096 in
  doc_to_buf buf doc;
  Buffer.contents buf

let doc_to_chan chan doc =
  let buf = Buffer.create 4096 in
  doc_to_buf buf doc;
  Buffer.output_buffer chan buf
