module FusionTree

public export
data FusionTree a = Leaf a | Node (FusionTree a) (FusionTree a)

public export
empty : FusionTree a
empty = Leaf 0

public export
isEmpty : FusionTree a -> Bool
isEmpty (Leaf 0) = True
isEmpty _        = False

public export
insert : Num a => a -> FusionTree a -> FusionTree a
insert x (Leaf 0) = Leaf x
insert x (Leaf y) = Node (Leaf x) (Leaf y)
insert x (Node left right) = Node (insert x left) right

public export
find : Num a => a -> FusionTree a -> a
find x (Leaf y) = y
find x (Node left right) = find x left + find x right

public export
union : FusionTree a -> FusionTree a -> FusionTree a
union (Leaf 0) t = t
union t (Leaf 0) = t
union (Leaf x) (Leaf y) = Node (Leaf x) (Leaf y)
union (Node ll lr) (Node rl rr) = Node (union ll rl) (union lr rr)