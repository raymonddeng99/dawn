type color = Red | Black

type 'a rbtree =
  | Empty
  | Node of color * 'a rbtree * 'a * 'a rbtree

let balance = function
  | Black, Node (Red, Node (Red, a, x, b), y, c), z, d
  | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d
  | Black, a, x, Node (Red, Node (Red, b, y, c), z, d)
  | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
    Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | color, a, x, b -> Node (color, a, x, b)

let insert x s =
  let rec ins = function
    | Empty -> Node (Red, Empty, x, Empty)
    | Node (color, a, y, b) ->
       if x < y
       then balance (color, ins a, y, b)
       else if x > y
       then balance (color, a, y, ins b)
       else Node (color, a, y, b)
  in
  match ins s with
  | Node (_, a, x, b) -> Node (Black, a, x, b)
  | Empty -> Empty