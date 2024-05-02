open Base

module AuxTree = struct
  type key = int
  type t = 
    | Empty
    | Node of t * key * t

  let empty = Empty

  let rec insert x t =
    match t with
    | Empty -> Node (Empty, x, Empty)
    | Node (l, y, r) ->
       if x < y then Node (insert x l, y, r)
       else Node (l, y, insert x r)

  let rec find x t =
    match t with
    | Empty -> false
    | Node (l, y, r) ->
       if x = y then true
       else if x < y then find x l
       else find x r
end

module TangoTree = struct
  type key = int
  type t = AuxTree.t list

  let empty = [AuxTree.empty]

  let rec find x t =
    match t with
    | [] -> false
    | aux :: rest ->
       if AuxTree.find x aux then true
       else find x rest

  let rec split x aux =
    match aux with
    | AuxTree.Empty -> (AuxTree.empty, AuxTree.empty)
    | AuxTree.Node (l, y, r) ->
       if x < y then
         let (ll, lr) = split x l in
         (ll, AuxTree.Node (lr, y, r))
       else
         let (rl, rr) = split x r in
         (AuxTree.Node (l, y, rl), rr)

  let rec join l r =
    match (l, r) with
    | (AuxTree.Empty, _) -> r
    | (_, AuxTree.Empty) -> l
    | (AuxTree.Node (ll, lx, lr), AuxTree.Node (rl, rx, rr)) ->
       if lx < rx then AuxTree.Node (ll, lx, join lr r)
       else AuxTree.Node (join l rl, rx, rr)

  let rec update x t =
    match t with
    | [] -> [AuxTree.Node (AuxTree.empty, x, AuxTree.empty)]
    | aux :: rest ->
       let (l, r) = split x aux in
       join l (AuxTree.Node (AuxTree.empty, x, AuxTree.empty)) :: rest
end