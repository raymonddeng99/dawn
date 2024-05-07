module YFastTrie
  ( Trie
  , empty
  , add
  , lookup
  ) where

data Trie a = Leaf | Node a (Trie a) (Trie a)

empty :: Trie a
empty = Leaf

add :: Ord a => a -> [a] -> Trie a -> Trie a
add x [] trie = Node x Leaf Leaf
add x (y:ys) Leaf = let n = Node y Leaf Leaf in Node y (add x ys n) Leaf
add x (y:ys) (Node z l r)
  | x < z = Node z (add x (y:ys) l) r
  | otherwise = Node z l (add x ys r)

lookup :: Ord a => [a] -> Trie a -> Maybe a
lookup [] (Node x _ _) = Just x
lookup (y:ys) Leaf = Nothing
lookup (y:ys) (Node x l r)
  | y < x = lookup (y:ys) l
  | otherwise = lookup ys r