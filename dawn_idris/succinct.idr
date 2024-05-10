module CompactSuffixArray

import Data.Vect
import Data.List
import Data.Char

rankSuffix : String -> (Char -> Nat) -> Nat -> Nat -> Nat
rankSuffix text rankOfChar n i =
  foldl (\r, j => r * cast (length rankOfChar) + rankOfChar (index j text)) 0 [i..n-1]

computeRankOfChar : String -> (Char -> Nat)
computeRankOfChar text =
  let ranks = sort $ nub text
      rankOfChar c = cast $ elemIndex c ranks
  in rankOfChar

compactSuffixArray : String -> Vect len Nat
compactSuffixArray text =
  let n = length text
      rankOfChar = computeRankOfChar text
      ranks = map (rankSuffix text rankOfChar (cast n)) [0..n-1]
      indices = map fst $ sort $ zip [0..n-1] ranks
  in indices