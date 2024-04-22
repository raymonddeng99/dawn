module SymbolSelector

import Data.Str
import Data.List
import Data.Vect

getngrams : Nat -> String -> List String
getngrams n s = [substr i n s | i <- [0..length s - n]]

count3gramfrequencies : List String -> Vect (String, Nat)
count3gramfrequencies sample_keys = fromList $ concat [[MkPair x 1 | x <- getngrams 3 key] | key <- sample_keys]
                                  |> group
                                  |> map (\xs => MkPair (fst $ head xs) (length xs))

select3gramintervals : List String -> Nat -> List (String, Maybe String)
select3gramintervals sample_keys dict_size =
  let frequencies = count3gramfrequencies sample_keys
      sorted_frequencies = sortBy (comparing snd) (reverse []) frequencies
      selected_ngrams = map fst $ take (dict_size `div` 2) sorted_frequencies
      intervals = foldl (\acc, ngram => case acc of
                                              [] => [(ngram, Nothing)]
                                              (last_ngram, _) :: rest =>
                                                  let start = last_ngram ++ (strCons (chr (cast (ord (last last_ngram) + 1))) "")
                                                  in (start, Just ngram) :: acc) [] (sort selected_ngrams)
  in intervals