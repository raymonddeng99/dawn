module BWT (encode, decode) where

import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)

lastChar :: String -> Char
lastChar s = last $ sort $ map rotate s
  where rotate xs = drop i xs ++ take i xs
          i = length xs

findIndex :: String -> Int
findIndex s = fromJust $ elemIndex s $ sort $ map rotate s
  where rotate xs = drop i xs ++ take i xs
          i = length xs

encode :: String -> (String, Int)
encode s = (encoded, idx)
  where n = length s
        rotations = sort $ map rotate [0..n-1]
        rotate i = drop i s ++ take i s
        encoded = map last rotations
        idx = findIndex s

decode :: (String, Int) -> String
decode (encoded, idx) = map last $ sort $ map rebuild [0..n-1]
  where n = length encoded
        rebuild i = take 1 (drop i encoded ++ lastChar (drop 0 i encoded))