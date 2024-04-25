open Base

(* On-line construction of suffix trees, Ukkoken '95 *)

type node = {
  start : int;
  len : int;
  children : (char, node) Hashtbl.t;
  suffixLink : node option;
  mutable parent : node option;
}

let root = { start = 0; len = 0; children = Hashtbl.create 10; suffixLink = None; parent = None }

let create_node start len parent =
  { start; len; children = Hashtbl.create 10; suffixLink = None; parent = Some parent }

let split_node node start len node_label =
  let new_node = create_node start len node in
  let remaining_len = node.len - len - 1 in
  node.len <- len;
  let child = create_node (node.start + len + 1) remaining_len new_node in
  Hashtbl.replace node.children node_label child;
  new_node.suffixLink <- node.suffixLink;
  node.suffixLink <- Some new_node;
  new_node

let rec find_node root start len =
  if len = 0 then
    Some root
  else
    match Hashtbl.find_opt root.children.[start] with
    | Some child ->
        let child_len = child.len in
        if child_len >= len then
          Some child
        else
          let remaining_len = len - child_len - 1 in
          find_node child (child.start + child_len + 1) remaining_len
    | None -> None

let rec follow_links node =
  match node.suffixLink with
  | Some link -> follow_links link
  | None -> node

let update_tree s i =
  let len = String.length s in
  let rec aux node i =
    let remaining = len - i in
    if remaining = 0 then
      ()
    else
      let leaf = create_node i remaining node in
      let node_label = s.[i] in
      match Hashtbl.find_opt node.children node_label with
      | Some child ->
          let start = child.start in
          let len = child.len in
          let found_node = find_node root start len in
          begin
            match found_node with
            | Some found_node ->
                if s.[start .. start + len] = s.[i .. i + len] then
                  aux child (i + len + 1)
                else
                  let new_node = split_node found_node start len node_label in
                  let leaf' = create_node i remaining new_node in
                  Hashtbl.replace new_node.children node_label leaf'
            | None ->
                Hashtbl.replace node.children node_label leaf
          end
      | None ->
          Hashtbl.replace node.children node_label leaf
  in
  aux (follow_links root) i

let build_suffix_tree s =
  let len = String.length s in
  for i = 0 to len - 1 do
    update_tree s i
  done;
  root