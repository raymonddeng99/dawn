module PriorityQueue
  ( PriorityQueue
  , empty
  , isEmpty
  , insert
  , findMin
  , removeMin
  ) where

import Data.Array (Array, (!), (//))
import qualified Data.Array as Array

data PriorityQueue a
  = Leaf (Maybe a)
  | Node (Array Int (PriorityQueue a))
  deriving (Show)

clusterSize :: Int
clusterSize = 2

emptyCluster :: PriorityQueue a
emptyCluster = Node (Array.listArray (0, clusterSize - 1) (replicate clusterSize (Leaf Nothing)))

empty :: PriorityQueue a
empty = Leaf Nothing

isEmpty :: PriorityQueue a -> Bool
isEmpty (Leaf Nothing) = True
isEmpty _              = False

insert :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insert x (Leaf Nothing) = Leaf (Just x)
insert x (Leaf (Just y)) = buildCluster [Leaf (Just x), Leaf (Just y)]
insert x (Node cluster) =
  let ins i pq =
        case pq of
          Leaf Nothing -> cluster // [(i, Leaf (Just x))]
          Leaf (Just y) ->
            let newCluster = emptyCluster
            in Node (newCluster // [(0, Leaf (Just y)), (1, Leaf (Just x))])
          Node cluster' ->
            let newCluster' = Array.listArray (0, clusterSize - 1) (map (ins 0) (Array.elems cluster'))
            in Node (newCluster' // [(i, Node cluster')])
  in Node (ins 0 (Node cluster))

findMin :: Ord a => PriorityQueue a -> Maybe a
findMin (Leaf Nothing) = Nothing
findMin (Leaf (Just x)) = Just x
findMin (Node cluster) = findMin (cluster ! 0)

removeMin :: Ord a => PriorityQueue a -> PriorityQueue a
removeMin (Leaf Nothing) = Leaf Nothing
removeMin (Leaf (Just _)) = Leaf Nothing
removeMin (Node cluster) =
  let rem i pq =
        case pq of
          Leaf Nothing -> Leaf Nothing
          Leaf (Just _) ->
            let newCluster = emptyCluster
            in Node (newCluster // [(0, Leaf Nothing)])
          Node cluster' ->
            let newCluster' = Array.listArray (0, clusterSize - 1) (map (rem 0) (Array.elems cluster'))
            in Node (newCluster' // [(i, Node cluster')])
  in Node (rem 0 (Node cluster))

buildCluster :: [PriorityQueue a] -> PriorityQueue a
buildCluster xs =
  let cluster = emptyCluster
      (Node cluster') = foldr (\x (Node c) -> Node (c // [(0, x)])) cluster xs
  in Node cluster'