module SignatureSort
  ( signatureSort
  ) where

import Data.Array.IO (IOArray, newArray, readArray, writeArray)

signatureSort :: Ord a => [a] -> [a]
signatureSort xs = mergeSort xs 0 (length xs)
  where
    mergeSort [] _ _ = []
    mergeSort xs l r
      | l >= r = xs
      | otherwise = let
          m = l + (r - l) `div` 2
          left = mergeSort xs l m
          right = mergeSort xs m r
        in
          merge left right
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y = x : merge xs (y:ys)
      | otherwise = let
          (zs, ws) = merge xs (y:ys)
        in
          y : zs ++ [x] ++ ws