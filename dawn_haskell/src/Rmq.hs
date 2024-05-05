module RMQ (
  Tree(..),
  fromTree,
  query
) where

import Data.Array
import Data.Bits

data Tree = Tree {
  values :: Array Int Int,
  euler :: Array Int Int,
  first :: Array Int Int,
  rmq :: Array (Array Int Int)
}

log2 :: Int -> Int
log2 n = go 0 n
  where
    go acc 1 = acc
    go acc n = go (acc + 1) (shiftR n 1)

precomputeRMQ :: Tree -> Tree
precomputeRMQ tree@(Tree _ euler first _) = Tree (values tree) euler first rmqValues
  where
    n = rangeSize euler
    k = log2 n + 1
    rmqValues = array ((0, 0), (k - 1, n - 1)) [((0, i), i) | i <- [0 .. n - 1]]
    go j i
      | i + (shiftL 1 (j - 1)) >= n = []
      | otherwise = let x = rmqValues ! (j - 1, i)
                        y = rmqValues ! (j - 1, i + (shiftL 1 (j - 1)))
                        minIndex = if euler ! x < euler ! y then x else y
                    in (j, i) : go j (i + (shiftL 1 j))
    rmqValues' = array ((0, 0), (k - 1, n - 1)) $ concatMap (go j) [0 .. n - (shiftL 1 j)] $ assocs rmqValues

fromTree :: Array Int Int -> Tree
fromTree values = precomputeRMQ Tree {
  values = values,
  euler = euler,
  first = first,
  rmq = array ((0, 0), (0, 0)) []
}
  where
    n = rangeSize values
    euler = array (0, 2 * n - 1) $ concat [[i, i + 1] | i <- [0, 2 .. 2 * n - 2]]
    first = array (0, 2 * n - 1) $ zip [0 ..] $ tail $ elems euler

query :: Tree -> Int -> Int -> Int
query (Tree values euler first rmq) l r = values ! (minimumBy (comparing (euler !)) [rmq ! (k, first ! l), rmq ! (k, (first ! r) - (shiftL 1 k) + 1)])
  where
    l' = min l r
    r' = max l r
    k = log2 (r' - l' + 1)