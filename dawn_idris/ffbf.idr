import Data.Vect

record BloomFilter where
  constructor MkBloomFilter
  size : Nat
  vector : Vect (size `div` 8 + 1) Bits8
  hashFuncs : List (String -> Nat)

createBloomFilter : (size : Nat) -> (hashFuncs : List (String -> Nat)) -> BloomFilter
createBloomFilter size hashFuncs =
  MkBloomFilter size (replicate (size `div` 8 + 1) 0) hashFuncs

addToBloomFilter : BloomFilter -> String -> BloomFilter
addToBloomFilter (MkBloomFilter size vector hashFuncs) element =
  let indices = map (\f => f element `mod` size) hashFuncs
      updateVector vec idx =
        let (beforeBytes, afterBytes) = splitAt (cast (idx `div` 8)) vec
            byte = index vec (idx `div` 8)
            mask = 1 `shiftL` (idx `mod` 8)
        in beforeBytes ++ (byte `xor` mask) :: afterBytes
  in MkBloomFilter size (foldl updateVector vector indices) hashFuncs

elemInBloomFilter : BloomFilter -> String -> Bool
elemInBloomFilter (MkBloomFilter size vector hashFuncs) element =
  all (\f => (index vector (f element `mod` size `div` 8) `and` (1 `shiftL` (f element `mod` size `mod` 8))) /= 0) hashFuncs