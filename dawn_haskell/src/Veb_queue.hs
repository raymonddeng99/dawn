-- On RAM priority queues, Thorup '96

data VEBTree a = Leaf a | Node (VEBTree a) (VEBTree a)

vebMinTree :: VEBTree a -> Maybe a
vebMinTree (Leaf x) = Just x
vebMinTree (Node l r) = case (vebMinTree l, vebMinTree r) of
                          (Just x, Just y) -> Just (min x y)
                          (Just x, Nothing) -> Just x
                          (Nothing, Just y) -> Just y
                          (Nothing, Nothing) -> Nothing

vebInsert :: Ord a => a -> VEBTree a -> VEBTree a
vebInsert x (Leaf y) = if x < y then Node (Leaf x) (Leaf y) else Node (Leaf y) (Leaf x)
vebInsert x (Node l r) = case vebMinTree l of
                           Just minL -> if x < minL then Node (vebInsert x l) r
                                       else Node l (vebInsert x r)
                           Nothing -> Node (vebInsert x l) r