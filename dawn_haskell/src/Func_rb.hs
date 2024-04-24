data Color = Red | Black deriving (Eq, Show)
data RBTree a = Empty | Node Color (RBTree a) a (RBTree a) deriving (Eq, Show)

balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance Black (Node Red (Node Red a x b) y c) z d =
  Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d =
  Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance color a x b = Node color a x b

insert :: Ord a => a -> RBTree a -> RBTree a
insert x Empty = Node Red Empty x Empty
insert x (Node color a y b)
  | x < y = balance color (insert x a) y b
  | x > y = balance color a y (insert x b)
  | otherwise = Node color a y b