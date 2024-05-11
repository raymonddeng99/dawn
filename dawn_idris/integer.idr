module SignatureSort

mergeSort : Ord a => List a -> List a
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    len = length xs
    mid = len `div` 2
    left = take mid xs
    right = drop mid xs

merge : Ord a => List a -> List a -> List a
merge [] ys = ys
merge xs [] = xs
merge (x :: xs) (y :: ys) =
  if x <= y
    then x :: merge xs (y :: ys)
    else y :: merge (x :: xs) ys