import System.Random (randomRIO)

universalHash :: (Int, Int, Int, Int, Int) -> Int -> Int
universalHash (a, b, c, d, e) key =
  let m = 2 ^ 32
      hash = a * key + b * key ^ 2 + c * key ^ 3 + d * key ^ 4 + e * key ^ 5
  in hash `mod` m

linearProbe :: [Maybe a] -> Int -> IO Int
linearProbe table key = do
  let m = length table
  (a, b, c, d, e) <- replicateM 5 (randomRIO (0, m - 1))
  let hash = universalHash (a, b, c, d, e) key
  let probe i = let index = (hash + i) `mod` m
                in if table !! index == Nothing
                   then return index
                   else probe (i + 1)
  probe 0