open Base

module SurfBase = struct
  module Map = Map.Make(String)

  let build keys =
    let rec build_trie trie = function
      | [] -> trie
      | key :: rest ->
         let len = String.length key in
         let rec insert trie i =
           if i = len then trie
           else
             let c = key.[i] in
             let trie' =
               match Map.find_opt c trie with
               | None -> Map.add c (Map.empty) trie
               | Some child -> Map.add c child trie
             in
             insert trie' (i + 1)
         in
         let trie' = insert trie 0 in
         build_trie trie' rest
    in
    build_trie Map.empty keys

  let lookup trie key =
    let rec lookup trie i =
      if i = String.length key then true
      else
        match Map.find_opt key.[i] trie with
        | None -> false
        | Some child -> lookup child (i + 1)
    in
    lookup trie 0
end