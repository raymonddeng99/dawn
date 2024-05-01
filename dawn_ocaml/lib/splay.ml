open Base

module SplayTree = struct
  type 'a tree =
    | Empty
    | Node of 'a tree * 'a * 'a tree

  let rec mem x = function
    | Empty -> false
    | Node (l, y, r) ->
       if x < y then mem x l
       else if x > y then mem x r
       else true

  let rec insert x = function
    | Empty -> Node (Empty, x, Empty)
    | Node (l, y, r) as t ->
       if x < y then splay (insert x l, y, r)
       else if x > y then splay (l, y, insert x r)
       else t

  and splay = function
    | Empty -> Empty
    | Node (Empty, x, r) -> Node (Empty, x, r)
    | Node (l, x, Empty) -> Node (l, x, Empty)
    | Node (l, x, Node (rl, y, rr)) ->
       if y < x then
         let l', r' = splay (l, x, rl) in
         Node (l', y, Node (r', x, rr))
       else
         let l', r' = splay (rl, y, rr) in
         Node (Node (l, x, l'), y, r')

  let rec remove x = function
    | Empty -> Empty
    | Node (l, y, r) as t ->
       if x < y then splay (remove x l, y, r)
       else if x > y then splay (l, y, remove x r)
       else join l r

  and join l r =
    match l, r with
    | Empty, t -> t
    | t, Empty -> t
    | Node (ll, x, lr), r' ->
       splay (ll, x, join lr r')
end