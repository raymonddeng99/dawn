-- On RAM priority queues, Thorup '96 

data VEBTree : Type -> Type where
  Leaf : a -> VEBTree a
  Node : VEBTree a -> VEBTree a -> VEBTree a

min : Ord a => VEBTree a -> Maybe a
min (Leaf x) = Just x
min (Node l r) = case (min l, min r) of
                   (Just x, Just y) => Just (min x y)
                   (Just x, Nothing) => Just x
                   (Nothing, Just y) => Just y
                   (Nothing, Nothing) => Nothing

insert : Ord a => a -> VEBTree a -> VEBTree a
insert x (Leaf y) = if x < y then Node (Leaf x) (Leaf y) else Node (Leaf y) (Leaf x)
insert x (Node l r) = case min l of
                        Just minL => if x < minL then Node (insert x l) r
                                     else Node l (insert x r)
                        Nothing => Node (insert x l) r

extractMin : Ord a => VEBTree a -> (Maybe a, VEBTree a)
extractMin (Leaf x) = (Just x, Leaf x)
extractMin (Node l r) = case (min l, min r) of
                          (Just minL, Just minR) =>
                            if minL < minR
                              then let (val, newL) = extractMin l in
                                   (val, Node newL r)
                              else let (val, newR) = extractMin r in
                                   (val, Node l newR)
                          (Just minL, Nothing) =>
                            let (val, newL) = extractMin l in
                              (val, Node newL r)
                          (Nothing, Just minR) =>
                            let (val, newR) = extractMin r in
                              (val, Node l newR)
                          (Nothing, Nothing) => (Nothing, Node l r)

decreaseKey : Ord a => a -> a -> VEBTree a -> VEBTree a
decreaseKey oldVal newVal (Leaf x) =
  if x == oldVal then Leaf newVal else Leaf x
decreaseKey oldVal newVal (Node l r) =
  let newL = decreaseKey oldVal newVal l
      newR = decreaseKey oldVal newVal r
      minV = min (Node newL newR)
  in case minV of
       Just minVal =>
         if newVal < minVal
           then Node (Leaf newVal) (Node newL newR)
           else Node newL newR
       Nothing => Node newL newR