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



-- Succinct static data structures, Jacobsen '89
module BitVector (BitVector, create, rank, select) where

import Data.Vector (Vector, fromList, (!?), (!))
import qualified Data.Vector as V

k :: Int
k = 6

rankNaive :: Vector Bool -> Int -> Int
rankNaive bits i = sum $ V.map (\b -> if b then 1 else 0) $ V.take (i + 1) bits

selectNaive :: Vector Bool -> Int -> Int
selectNaive bits i = go 0 0
  where
    go j cnt
      | cnt == i = j
      | j >= V.length bits = -1
      | otherwise = go (j + 1) (cnt + if bits ! j then 1 else 0)

create :: [Bool] -> BitVector
create bits = BitVector bits rankTable selectTable
  where
    n = length bits
    rankTable = V.generate (n + 1) (\i -> if i `mod` (1 `shiftL` k) == 0 then 0 else rankTable V.! (i - (i `mod` (1 `shiftL` k))) + rankNaive (V.fromList bits) i - rankNaive (V.fromList bits) (i - (i `mod` (1 `shiftL` k))))
    selectTable = V.generate (n + 1) (\i -> if i `mod` (1 `shiftL` k) == 0 then -1 else selectTable V.! (i - (i `mod` (1 `shiftL` k))) + selectNaive (V.fromList bits) i - selectNaive (V.fromList bits) (i - (i `mod` (1 `shiftL` k))))

data BitVector = BitVector (Vector Bool) (Vector Int) (Vector Int)

rank :: BitVector -> Int -> Int
rank (BitVector _ rankTable _) i = rankTable V.! i

select :: BitVector -> Int -> Int
select (BitVector bits _ selectTable) i = go 0 (V.length bits)
  where
    go l r
      | l > r = -1
      | otherwise = let m = l + (r - l) `div` 2 in
                    if rank (BitVector bits rankTable selectTable) m < i
                    then go (m + 1) r
                    else go l (m - 1)