data FusionTree a = Leaf a | Node (FusionTree a) (FusionTree a)

empty :: Num a => FusionTree a
empty = Leaf 0

isEmpty :: FusionTree a -> Bool
isEmpty (Leaf 0) = True
isEmpty _ = False

insert :: Num a => a -> FusionTree a -> FusionTree a
insert x (Leaf 0) = Leaf x
insert x (Leaf y) = Node (Leaf x) (Leaf y)
insert x (Node left right) = Node (insert x left) right

find :: Num a => a -> FusionTree a -> a
find x (Leaf y) = y
find x (Node left right) = find x left + find x right

union :: FusionTree a -> FusionTree a -> FusionTree a
union (Leaf 0) t = t
union t (Leaf 0) = t
union (Leaf x) (Leaf y) = Node (Leaf x) (Leaf y)
union (Node ll lr) (Node rl rr) = Node (union ll rl) (union lr rr)