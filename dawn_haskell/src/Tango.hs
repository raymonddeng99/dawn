module AuxTree (Tree, empty, insert, find) where

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving (Show)

empty :: Tree a
empty = Empty

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node left y right)
    | x < y     = Node (insert x left) y right
    | otherwise = Node left y (insert x right)

find :: Ord a => a -> Tree a -> Bool
find x Empty = False
find x (Node left y right)
    | x == y    = True
    | x < y     = find x left
    | otherwise = find x right




module TangoTree (TangoTree, empty, find, update) where

import AuxTree (Tree, empty, insert, find)

type TangoTree a = [Tree a]

empty :: TangoTree a
empty = [empty]

find :: Ord a => a -> TangoTree a -> Bool
find x = any (find x) 

split :: Ord a => a -> Tree a -> (Tree a, Tree a)
split x Empty = (Empty, Empty)
split x (Node left y right)
    | x < y     = (left1, Node left2 y right)
    | otherwise = (Node left y right1, right2)
    where
        (left1, left2) = split x left
        (right1, right2) = split x right

join :: Tree a -> Tree a -> Tree a
join Empty right = right
join left Empty = left
join left@(Node ll x lr) right@(Node rl y rr)
    | x < y     = Node ll x (join lr right)
    | otherwise = Node (join left rl) y rr

update :: Ord a => a -> TangoTree a -> TangoTree a
update x [] = [Node Empty x Empty]
update x (aux:rest) = join left (Node Empty x Empty) : rest
    where
        (left, right) = split x aux