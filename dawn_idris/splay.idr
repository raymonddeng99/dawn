module SplayTree

data SplayTree : Type -> Type where
  Empty : Ord a => SplayTree a
  Node : Ord a => (left : SplayTree a) -> (value : a) -> (right : SplayTree a) -> SplayTree a

splay : Ord a => SplayTree a -> SplayTree a
splay Empty = Empty
splay (Node l x r) = splayHelper (Node l x r) Empty

splayHelper : Ord a => SplayTree a -> SplayTree a -> SplayTree a
splayHelper t Empty = t
splayHelper t (Node l x r) =
  case splayHelper l r of
    Node ll y rl =>
      if y < x
        then Node (splayHelper ll Empty) y (Node rl x r)
        else Node (Node ll x rl) y r
    _ => t

insert : Ord a => a -> SplayTree a -> SplayTree a
insert x t = splay (insertHelper x t)

insertHelper : Ord a => a -> SplayTree a -> SplayTree a
insertHelper x Empty = Node Empty x Empty
insertHelper x (Node l y r) =
  if x < y
    then Node (insertHelper x l) y r
    else Node l y (insertHelper x r)

member : Ord a => a -> SplayTree a -> Bool
member x t = case splay t of
  Node l y r => x == y
  _ => False

delete : Ord a => a -> SplayTree a -> SplayTree a
delete x t = case splay t of
  Node l y r =>
    if x == y
      then merge l r
      else if x < y
        then Node (delete x l) y r
        else Node l y (delete x r)
  _ => t

merge : Ord a => SplayTree a -> SplayTree a -> SplayTree a
merge Empty t = t
merge t Empty = t
merge l (Node rl x rr) = splay (Node (merge l rl) x rr)