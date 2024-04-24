import Data.Nat

data Color = Red | Black

data RBTree : Color -> Type -> Type where
     Empty : RBTree Black a
     Node : Color
          -> RBTree c a
          -> a
          -> RBTree c a
          -> RBTree c a

balance : RBTree Black a -> a -> RBTree Black a -> RBTree Black a
balance (Node Red (Node Red a x b) y c) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Red a x (Node Red b y c)) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Red a x b) y (Node Red c z d) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Red a x b) y (Node Red (Node Red b y c) z d) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance c a x b = Node c a x b

insert : Ord a => a -> RBTree c a -> RBTree Black a
insert x Empty = Node Red Empty x Empty
insert x (Node c a y b) =
    case isLT x y of
         Yes prf => balance c (insert x a) y b
         No nPrf =>
            case isGT x y of
                 Yes prf => balance c a y (insert x b)
                 No nPrf => Node c a y b