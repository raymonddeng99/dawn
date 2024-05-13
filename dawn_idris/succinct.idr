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


-- Succinct static data structures, Jacobsen '89
module BitVector

import Data.Vect

%default total

k : Nat
k = 6

rankNaive : Vect n Bool -> Nat -> Nat
rankNaive bits i = sum $ map (\b => if b then 1 else 0) $ take (i + 1) bits

selectNaive : Vect n Bool -> Nat -> Maybe Nat
selectNaive bits i = go 0 0
  where
    go : Nat -> Nat -> Maybe Nat
    go j cnt = if cnt == i
                  then Just j
                  else if j >= length bits
                       then Nothing
                       else go (j + 1) (cnt + if index bits j then 1 else 0)

create : Vect n Bool -> (Vect n Bool, Vect (n + 1) Nat, Vect (n + 1) (Maybe Nat))
create bits = (bits, rankTable, selectTable)
  where
    n : Nat
    n = length bits
    rankTable : Vect (n + 1) Nat
    rankTable = tabulate (\i => if i `mod` (1 `shiftL` k) == 0
                                   then 0
                                   else let prev = i - (i `mod` (1 `shiftL` k))
                                        in rankTable ! prev + rankNaive bits i - rankNaive bits prev)
    selectTable : Vect (n + 1) (Maybe Nat)
    selectTable = tabulate (\i => if i `mod` (1 `shiftL` k) == 0
                                    then Nothing
                                    else let prev = i - (i `mod` (1 `shiftL` k))
                                         in maybe Nothing (\j => Just (j + selectNaive bits i - selectNaive bits prev)) (selectTable ! prev))

rank : (Vect n Bool, Vect (n + 1) Nat, Vect (n + 1) (Maybe Nat)) -> Nat -> Nat
rank (_, rankTable, _) i = index rankTable i

select : (Vect n Bool, Vect (n + 1) Nat, Vect (n + 1) (Maybe Nat)) -> Nat -> Maybe Nat
select (bits, rankTable, selectTable) i = go 0 (length bits)
  where
    go : Nat -> Nat -> Maybe Nat
    go l r = if l > r
                then Nothing
                else let m = l + (r - l) `div` 2
                         res = rank (bits, rankTable, selectTable) m
                     in if res < i
                           then go (m + 1) r
                           else go l (m - 1)