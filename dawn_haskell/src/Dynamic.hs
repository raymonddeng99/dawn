module EulerTourTree where

import Data.Tree

data EulerTourTree a = EulerTourTree [a] [Int] [Int] [Int] deriving (Show, Eq)

buildEulerTour :: Tree a -> [a]
buildEulerTour (Node x forest) = x : concatMap buildEulerTour forest ++ [x]
buildEulerTour (Node _ []) = []

buildEulerTourTree :: [a] -> EulerTourTree a
buildEulerTourTree tour = EulerTourTree tour first last level
  where
    n = length tour
    first = buildFirst tour
    last = buildLast tour
    level = buildLevel tour

    buildFirst tour = buildFirst' tour 0 [] (-1)
    buildFirst' [] _ acc _ = reverse acc
    buildFirst' (x:xs) i acc j
      | i == 0 = buildFirst' xs (i + 1) (0 : acc) 0
      | otherwise = case j of
          (-1) -> buildFirst' xs (i + 1) (i : acc) i
          _    -> buildFirst' xs (i + 1) acc j

    buildLast tour = buildLast' tour (n - 1) []
    buildLast' [] _ acc = reverse acc
    buildLast' (x:xs) i acc =
      let j = findLast x xs i
      in buildLast' xs j (j : acc)

    buildLevel tour = buildLevel' tour 0 []
    buildLevel' [] _ acc = reverse acc
    buildLevel' (x:xs) i acc =
      let j = findLast x xs i
      in if j == -1
           then buildLevel' xs (i + 1) (0 : acc)
           else buildLevel' xs j ((i - j) : acc)

    findLast x [] i = -1
    findLast x (y:ys) i
      | x == y = i
      | otherwise = findLast x ys (i - 1)