module LinkCutTree

import Data.Vect

record Node where
  constructor MkNode
  value   : Int
  parent  : Maybe Node
  left    : Maybe Node
  right   : Maybe Node
  rev     : Bool
  sum     : Int

makeNode : Int -> Node
makeNode value = MkNode value Nothing Nothing Nothing False value

getSum : Maybe Node -> Int
getSum Nothing  = 0
getSum (Just n) = n.sum

updateSum : Node -> Node
updateSum n = record { sum = n.value + getSum n.left + getSum n.right } n

push : Node -> Node
push n =
  if n.rev
    then record { rev = False,
                  left = n.right,
                  right = n.left,
                  left = map pushChild n.left,
                  right = map pushChild n.right } n
    else n
  where
    pushChild : Maybe Node -> Maybe Node
    pushChild Nothing = Nothing
    pushChild (Just c) = Just $ record { rev = not c.rev } (push c)

makeRoot : Node -> Node
makeRoot n = go n
  where
    go : Node -> Node
    go n' = let n'' = push n' in
            case n''.parent of
              Nothing => n''
              Just p => let p' = record { left = Nothing, right = Nothing } p in
                         go $ record { parent = Nothing } n''

splay : Node -> Node
splay n = makeRoot n |> go
  where
    go : Node -> Node
    go n' =
      case n'.parent of
        Nothing => n'
        Just p =>
          let p' = makeRoot p in
          case p'.parent of
            Nothing => rot p' n'
            Just pp =>
              let pp' = makeRoot pp in
              if (pp'.left == Just p') == (p'.left == Just