module SplayTree
  ( SplayTree
  , empty
  , insert
  , member
  , delete
  ) where

data SplayTree a = Empty | Node (SplayTree a) a (SplayTree a)
  deriving (Show)

empty :: SplayTree a
empty = Empty

member :: Ord a => a -> SplayTree a -> Bool
member x Empty = False
member x (Node l y r)
  | x < y = member x l
  | x > y = member x r
  | otherwise = True

insert :: Ord a => a -> SplayTree a -> SplayTree a
insert x t = splay (insertHelper x t)

insertHelper :: Ord a => a -> SplayTree a -> SplayTree a
insertHelper x Empty = Node Empty x Empty
insertHelper x (Node l y r)
  | x < y = splay (insertHelper x l, y, r)
  | x > y = splay (l, y, insertHelper x r)
  | otherwise = Node l x r

splay :: Ord a => (SplayTree a, a, SplayTree a) -> SplayTree a
splay (Empty, x, r) = Node Empty x r
splay (l, x, Empty) = Node l x Empty
splay (l, x, Node rl y rr)
  | y < x = let l' = splay (l, x, rl)
                r' = Node rl y rr
            in Node l' y r'
  | otherwise = let r' = splay (rl, y, rr)
                   l' = Node l x rl
               in Node l' y r'

delete :: Ord a => a -> SplayTree a -> SplayTree a
delete x t = splay (deleteHelper x t)

deleteHelper :: Ord a => a -> SplayTree a -> (SplayTree a, a, SplayTree a)
deleteHelper x Empty = (Empty, x, Empty)
deleteHelper x (Node l y r)
  | x < y = let (l', x', r') = deleteHelper x l
            in (l', x', Node r' y r)
  | x > y = let (l', x', r') = deleteHelper x r
            in (Node l y l', x', r')
  | otherwise = (l, y, r)