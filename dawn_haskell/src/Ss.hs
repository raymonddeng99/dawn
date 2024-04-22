module SymbolSelector where

import qualified Data.List as L
import qualified Data.Map as M

get_ngrams n s = [take n (drop i s) | i <- [0..(length s - n + 1)]]

count_3gram_frequencies sample_keys = M.fromListWith (+) [(ngram, 1) | key <- sample_keys, ngram <- get_ngrams 3 key]

select_3gram_intervals sample_keys dict_size =
  let frequencies = count_3gram_frequencies sample_keys
      sorted_frequencies = L.sortBy (\(_, freq1) (_, freq2) -> compare freq2 freq1) $ M.toList frequencies
      selected_ngrams = map fst $ take (dict_size `div` 2) sorted_frequencies
      intervals = foldr (\ngram acc -> case acc of
                           [] -> [(ngram, Nothing)]
                           (last_ngram, _):rest -> let start = last_ngram ++ [chr $ ord (last last_ngram) + 1]
                                                    in (start, Just ngram):acc) [] $ L.sort selected_ngrams
  in intervals