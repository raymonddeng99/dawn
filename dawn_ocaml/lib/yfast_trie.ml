open Base

module YFastTrie : sig
  type 'a t
  val empty : 'a t
  val add : 'a -> 'a list -> 'a t -> 'a t
  val lookup : 'a list -> 'a t -> 'a option
end = struct
  type 'a node =
    | Leaf
    | Node of 'a * 'a t * 'a t
  and 'a t = 'a node ref

  let empty = ref Leaf

  let rec add x xs trie =
    match !trie, xs with
    | Leaf, [] -> trie := Node (x, ref Leaf, ref Leaf)
    | Leaf, y :: ys ->
      let n = ref Leaf in
      trie := Node (y, n, ref Leaf);
      add x ys n
    | Node (y, l, r), [] -> trie := Node (y, l, r)
    | Node (y, l, r), z :: zs ->
      if x < y then
        add x (z :: zs) l
      else
        add x zs r

  let rec lookup xs trie =
    match !trie, xs with
    | Leaf, [] -> None
    | Leaf, _ -> None
    | Node (y, l, r), [] -> Some y
    | Node (y, l, r), z :: zs ->
      if z < y then lookup (z :: zs) l
      else lookup zs r
end