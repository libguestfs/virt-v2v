(* Given a sorted list of pathnames (eg. from [find / | LANG=C sort])
 * list the directories containing most files.  Useful for
 * diagnosing out of control SELinux relabelling / setfiles problems.
 *)

open Printf

let input =
  if Array.length Sys.argv <> 2 then
    failwith "usage: ./setfiles_analysis sorted_paths.txt";
  Sys.argv.(1)

let chan = open_in input

(* Get the next path from the file. *)
let get () =
  try
    let line = input_line chan in
    assert (String.length line > 0);
    assert (line.[0] = '/');
    let path = String.split_all ~sep:"/" line in
    assert (List.length path > 0);
    assert (List.hd path = "");
    let path = List.tl path in
    Some path
  with End_of_file ->
    None

(* Mutable tree structure, used temporarily. *)
type mtree = (string, mnode) Hashtbl.t
and mnode = MFile | MDirectory of mtree

let rec insert path tree =
  match path with
  | [] -> ()
  | h :: tl ->
     let node = Hashtbl.find_opt tree h in
     match node with
     | None ->
        Hashtbl.add tree h MFile
     | Some MFile ->
        let dir = Hashtbl.create 16 in
        insert tl dir;
        Hashtbl.replace tree h (MDirectory dir)
     | Some (MDirectory dir) ->
        insert tl dir

let mtree : mtree = Hashtbl.create 16

(* Read in the tree. *)
let () =
  let rec loop () =
    match get () with
    | Some path -> insert path mtree; loop ()
    | None -> ()
  in
  loop ()

(* Normal tree structure. *)
type 'a tree = (string * 'a node) list
and 'a node = File | Directory of 'a * 'a tree

(* Convert temporary tree to normal tree. *)
let tree : unit tree =
  let rec loop mtree =
    Hashtbl.fold (
      fun name mnode tree ->
        let node =
          match mnode with
          | MFile -> File
          | MDirectory mtree ->
             Directory ((), List.sort compare (loop mtree)) in
        (name, node) :: tree
    ) mtree []
  in
  loop mtree

let () =
  if false then (
    let rec loop indent tree =
      List.iter (
      function
      | name, File -> printf "%s%S, File\n" indent name
      | name, Directory ((), tree) ->
         printf "%s%S, Directory\n" indent name;
         loop (indent ^ "  ") tree
      ) tree
    in
    loop "" tree
  )

(* Annotate each node with the number of children and path
 * of each node.
 *)
let annot_tree : (int * string) tree =
  let rec loop path tree =
    List.map (
    function
    | name, File -> name, File
    | name, Directory ((), tree) ->
       let path = path ^ "/" ^ name in
       let tree = loop path tree in
       let nr_children =
         List.fold_left (
           fun nr_children (_, node) ->
             match node with
             | File -> nr_children + 1
             | Directory ((n, _), tree) -> nr_children + n
         ) 0 tree in
       name, Directory ((nr_children, path), tree)
    ) tree
  in
  loop "" tree

let () =
  if false then (
    let rec loop indent tree =
      List.iter (
      function
      | name, File -> printf "%s%S, File\n" indent name
      | name, Directory ((nr_children, path), tree) ->
         printf "%s%S, Directory (%d, %S)\n" indent name nr_children path;
         loop (indent ^ "  ") tree
      ) tree
    in
    loop "" annot_tree
  )

(* Flatten (only directories) and reverse sort by number of children. *)
let flattened_dirs : (int * string) list =
  let r = ref [] in
  let rec loop tree =
    List.iter (
    function
    | _, File -> ()
    | _, Directory ((nr_children, path), tree) ->
       loop tree;
       r := (nr_children, path) :: !r
    ) tree
  in
  loop annot_tree;
  !r

let sorted_dirs =
  List.sort (fun (n1, _) (n2, _) -> compare n2 n1) flattened_dirs

(* Print the result. *)
let () =
  List.iter (
    fun (nr_children, path) ->
      let path = if path = "" then "/" else path in
      printf "%-10d %s\n" nr_children path
  ) sorted_dirs
