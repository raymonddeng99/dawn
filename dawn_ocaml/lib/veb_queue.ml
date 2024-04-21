(* On RAM priority queues, Thorup '96 *)

open Base

type 'a veb_tree =
  | Leaf of 'a
  | Node of 'a veb_tree * 'a veb_tree

let veb_min_tree (t : 'a veb_tree) : 'a option =
  match t with
  | Leaf x -> Some x
  | Node (l, r) ->
     begin match veb_min_tree l, veb_min_tree r with
     | Some x, Some y -> Some (min x y)
     | Some x, None -> Some x
     | None, Some y -> Some y
     | None, None -> None
     end

let veb_insert (x : 'a) (t : 'a veb_tree) : 'a veb_tree =
  match t with
  | Leaf y -> if x < y then Node (Leaf x, Leaf y) else Node (Leaf y, Leaf x)
  | Node (l, r) ->
     if x < (veb_min_tree l).Contents.val then Node (veb_insert x l, r)
     else Node (l, veb_insert x r)