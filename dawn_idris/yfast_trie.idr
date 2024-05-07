module YFastTrie

%default total

data Trie : Type -> Type where
  Leaf : Trie a
  Node : (val : a) -> Trie a -> Trie a -> Trie a

empty : Trie a
empty = Leaf

add : Ord a => a -> List a -> Trie a -> Trie a
add x [] trie = Node x Leaf Leaf
add x (y :: ys) Leaf = let n = Node y Leaf Leaf in Node y (add x ys n) Leaf
add x (y :: ys) (Node z l r) =
  case compare x z of
    LT => Node z (add x (y :: ys) l) r
    EQ => Node z l r
    GT => Node z l (add x ys r)

lookup : Ord a => List a -> Trie a -> Maybe a
lookup [] (Node x _ _) = Just x
lookup _ Leaf = Nothing
lookup (y :: ys) (Node x l r) =
  case compare y x of
    LT => lookup (y :: ys) l
    EQ => lookup ys r
    GT => lookup ys r