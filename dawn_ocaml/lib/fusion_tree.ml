open Base

module FusionTree = struct
    type 'a tree =
        | Leaf of 'a
        | Node of 'a tree * 'a tree

    let empty = Leaf 0

    let is_empty = function
        | Leaf 0 -> true
        | _ -> false

    let rec insert x = function
        | Leaf 0 -> Leaf x
        | Leaf y -> Node (Leaf x, Leaf y)
        | Node (left, right) ->
            let left' = insert x left in
            let right' = right in
            Node (left', right')

    let rec find x = function
        | Leaf y -> y
        | Node (left, right) ->
            let left_val = find x left in
            let right_val = find x right in
            left_val + right_val

    let rec union left right =
        match left, right with
        | Leaf 0, t
        | t, Leaf 0 -> t
        | Leaf x, Leaf y -> Node (Leaf x, Leaf y)
        | Node (ll, lr), Node (rl, rr) ->
            let new_left = union ll rl in
            let new_right = union lr rr in
            Node (new_left, new_right)
end