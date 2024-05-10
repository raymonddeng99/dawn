module SuffixArray
  ( SuffixArray
  ) where

import           Data.Array
import           Data.Char (ord)
import           Data.List (sortBy)

rankSuffix :: String -> (Char -> Int) -> Int -> Int -> Int
rankSuffix text rankOfChar n i =
  foldr (\j r -> r * rankOfChar (text !! j) + rankOfChar (text !! i)) 0 [i..n-1]

computeRankOfChar :: String -> (Char -> Int)
computeRankOfChar text = accumArray (+) 0 (0, 255) [(ord c, 1) | c <- text] // arr
  where
    arr = accumArray (const id) 0 (0, 255) [(ord c, head [i | i <- [0..] , arr ! i == 0]) | c <- nub text]

suffixArray :: String -> [Int]
suffixArray text =
  let n = length text
      rankOfChar = computeRankOfChar text
      ranks = listArray (0, n-1) [rankSuffix text rankOfChar n i | i <- [0..n-1]]
  in
    map fst $ sortBy (comparing snd) $ zip [0..n-1] (elems ranks)