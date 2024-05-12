module Deque (Deque, empty, isEmpty, addFront, addBack, takeFront, takeBack) where

data Node a = One a | Two a a | Three a a a | Four a a a a
  deriving (Show, Eq)

data Deque a = Deque [Node a] [Node a]
  deriving (Show, Eq)

empty :: Deque a
empty = Deque [] []

isEmpty :: Deque a -> Bool
isEmpty (Deque [] []) = True
isEmpty _ = False

addFront :: a -> Deque a -> Deque a
addFront x (Deque front rear) =
  let rev_nodes [] = [One x]
      rev_nodes (One x : xs) = Four x x x x : rev_nodes xs
      rev_nodes (Two x y : xs) = Three x x y : Four y y y y : rev_nodes xs
      rev_nodes (Three x y z : xs) = Two x y : Two y z : Four z z z z : rev_nodes xs
      rev_nodes (Four w x y z : xs) = One w : One x : One y : One z : rev_nodes xs
  in Deque (rev_nodes front) (reverse rear)

addBack :: a -> Deque a -> Deque a
addBack x (Deque front rear) =
  let rev_nodes [] = [One x]
      rev_nodes (One x : xs) = Four x x x x : rev_nodes xs
      rev_nodes (Two x y : xs) = Three x y y : Four x x x x : rev_nodes xs
      rev_nodes (Three x y z : xs) = Two x y : Two z z : Four y y y y : rev_nodes xs
      rev_nodes (Four w x y z : xs) = One w : One x : One y : One z : rev_nodes xs
  in Deque front (rev_nodes rear)

takeFront :: Deque a -> (Maybe a, Deque a)
takeFront (Deque [] _) = (Nothing, Deque [] [])
takeFront (Deque (One x : front') rear) =
  let rear' = case rear of
        [] -> []
        [One x] -> []
        nodes -> reverse nodes
  in (Just x, Deque front' rear')
takeFront (Deque (Two x y : front') rear) =
  let rear' = case rear of
        [] -> []
        [Two x y] -> [One x, One y]
        One x : nodes -> reverse (Two x y : nodes)
        _ -> reverse rear
  in (Just x, Deque (One y : front') rear')
takeFront (Deque (Three x y z : front') rear) =
  let rear' = case rear of
        [] -> []
        [Three x y z] -> [One x, One y, One z]
        One x : nodes -> reverse (Three x y z : nodes)
        Two x y : nodes -> reverse (Two x y : Two y z : nodes)
        _ -> reverse rear
  in (Just x, Deque (Two y z : front') rear')
takeFront (Deque (Four w x y z : front') rear) =
  let rear' = case rear of
        [] -> []
        [Four w x y z] -> [One w, One x, One y, One z]
        One x : nodes -> reverse (Four w x y z : nodes)
        Two x y : nodes -> reverse (Two x y : Two y z : Two z w : nodes)
        Three x y z : nodes -> reverse (Three x y z : Two z w : One w : nodes)
        _ -> reverse rear
  in (Just w, Deque (Three x y z : front') rear')

takeBack :: Deque a -> (Maybe a, Deque a)
takeBack (Deque front []) = (Nothing, Deque front [])
takeBack (Deque front (One x : rear')) =
  let front' = case reverse front of
        [] -> []
        One x : front'' -> reverse front''
        Two x y : front'' -> reverse (One y : front'')
        Three x y z : front'' -> reverse (Two y z : front'')
        Four w x y z : front'' -> reverse (Three x y z : front'')
  in (Just x, Deque front' rear')
takeBack (Deque front (Two x y : rear')) =
  let front' = case reverse front of
        [] -> []
        One x : front'' -> reverse (Two y x : front'')
        Two x z : front'' -> reverse (Three z y x : front'')
        Three w x z : front'' -> reverse (Four z x w y : front'')
        Four w x y z : front'' -> reverse (Three z y x : Two w y : One w : front'')
  in (Just y, Deque front' rear')
takeBack (Deque front (Three x y z : rear')) =
  let front' = case reverse front of
        [] -> []
        One x : front'' -> reverse (Three z y x : front'')
        Two x w : front'' -> reverse (Four w z y x : front'')
        Three w x y : front'' -> reverse (Two y x : Two w z : One w : front'')
        Four w x y z : front'' -> reverse (One z : Three y x w : front'')
  in (Just z, Deque front' rear')
takeBack (Deque front (Four w x y z : rear')) =
  let front' = case reverse front of
        [] -> []
        One x : front'' -> reverse (Four z y x w : front'')
        Two x y : front'' -> reverse (Two z y : Two y x : One w : front'')
        Three x y z : front'' -> reverse (One z : Two y x : One w : front'')
        Four w' x' y' z' : front'' -> reverse (Three z' y' x' : front'')
  in (Just z, Deque front' rear')


-- Partially retroactive priority queue
module PriorityQueue (
  PQueue,
  create,
  isEmpty,
  insert,
  findMin,
  deleteMin,
  retroactiveUpdate
) where

import Data.List (sort, sortBy)
import Data.Ord (comparing)

data PQueue a = PQueue [(a, Int)] Int deriving (Show)

create :: PQueue a
create = PQueue [] 0

isEmpty :: PQueue a -> Bool
isEmpty (PQueue [] _) = True
isEmpty _              = False

insert :: Ord a => PQueue a -> a -> PQueue a
insert (PQueue data time) x = PQueue ((x, time + 1) : data) (time + 1)

findMin :: Ord a => PQueue a -> Maybe a
findMin (PQueue [])     = Nothing
findMin (PQueue data _) = Just $ fst $ head $ sortBy (comparing snd) data

deleteMin :: Ord a => PQueue a -> Maybe (PQueue a, a)
deleteMin (PQueue [])     = Nothing
deleteMin (PQueue data t) = Just (PQueue data' t, minVal)
  where
    (minVal, _) = head $ sortBy (comparing snd) data
    data'       = filter (\(x, _) -> x /= minVal) data

retroactiveUpdate :: Ord a => PQueue a -> Int -> a -> PQueue a
retroactiveUpdate (PQueue data time) t x = PQueue data' time
  where
    data' = map (\(v, t') -> if t' <= t then (x, t') else (v, t')) data


-- Simple Confluently Persistent Catenable Lists, Tarjan et al
module PersistentList
  ( PersistentList
  , empty
  , isEmpty
  , singleton
  , cons
  , head
  , tail
  , catenate
  ) where

data Node a = Node a (PersistentList a) (PersistentList a)
data PersistentList a = Nil | Cons (Node a)

empty :: PersistentList a
empty = Nil

isEmpty :: PersistentList a -> Bool
isEmpty Nil = True
isEmpty _   = False

singleton :: a -> PersistentList a
singleton x = Cons (Node x Nil Nil)

cons :: a -> PersistentList a -> PersistentList a
cons x Nil = singleton x
cons x (Cons n@(Node _ Nil Nil)) = Cons (Node x Nil (Cons n))
cons x (Cons (Node v l r)) = Cons (Node x Nil (Cons (Node v l r)))

head :: PersistentList a -> Maybe a
head Nil = Nothing
head (Cons (Node x _ _)) = Just x

tail :: PersistentList a -> PersistentList a
tail Nil = Nil
tail (Cons (Node _ Nil Nil)) = Nil
tail (Cons (Node _ l r)) = catenate l r

catenate :: PersistentList a -> PersistentList a -> PersistentList a
catenate Nil ys = ys
catenate xs Nil = xs
catenate xs@(Cons (Node _ _ Nil)) ys = xs `catenateHelp` ys
catenate xs ys@(Cons (Node _ Nil _)) = ys `catenateHelp` xs
catenate xs@(Cons (Node _ xl xr)) ys@(Cons (Node _ yl yr))
  | xr == yl  = Cons (Node (nodeValue xr) xl yr)
  | otherwise = xs `catenateHelp` ys

catenateHelp :: PersistentList a -> PersistentList a -> PersistentList a
catenateHelp Nil ys = ys
catenateHelp xs Nil = xs
catenateHelp xs@(Cons (Node x xl xr)) ys =
  Cons (Node x xl (catenateHelp xr ys))

nodeValue :: PersistentList a -> a
nodeValue (Cons (Node x _ _)) = x