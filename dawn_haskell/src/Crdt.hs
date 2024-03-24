-- (* CRDTs *)

{-
Specification: CRDTs = state-based | op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
-}


import Data.List (foldl')

data Operation = Increment | Decrement deriving (Show, Eq)

counter :: (Int, [Operation]) -> Operation -> (Int, [Operation])
counter (value, ops) op =
    let newValue = case op of
                     Increment -> value + 1
                     Decrement -> value - 1
        newOps = op : ops
    in (newValue, newOps)

initial :: (Int, [Operation])
initial = (0, [])

apply :: (Int, [Operation]) -> (Int, [Operation])
apply (value, ops) = (foldl' (\v op -> fst $ counter (v, []) op) value ops, [])

merge :: (Int, [Operation]) -> (Int, [Operation]) -> (Int, [Operation])
merge (value1, ops1) (value2, ops2) =
    let mergedOps = ops1 ++ ops2
        finalValue = foldl' (\v op -> fst $ counter (v, []) op) (max value1 value2) mergedOps
    in (finalValue, [])

downstream :: [Operation]
downstream = []

update :: Operation -> (Int, [Operation]) -> (Int, [Operation])
update op (value, ops) = counter (value, ops) op

-- State based increment-only counter
module GCounter (
    Counter,
    create,
    update,
    query,
    compare,
    merge
) where

type Counter = [Int]

create :: Int -> Counter
create size = replicate size 0

update :: Counter -> Int -> Counter
update counter i
    | i < 0 || i >= length counter = error "Index out of bounds"
    | otherwise = take i counter ++ [counter !! i + 1] ++ drop (i + 1) counter

query :: Counter -> Int -> Int
query counter i
    | i < 0 || i >= length counter = error "Index out of bounds"
    | otherwise = counter !! i

compare :: Counter -> Counter -> Ordering
compare counter1 counter2
    | length counter1 /= length counter2 = error "Vectors have different lengths"
    | otherwise = compareHelper 0 counter1 counter2
    where
        compareHelper i c1 c2
            | i >= length c1 = EQ
            | c1 !! i == c2 !! i = compareHelper (i + 1) c1 c2
            | c1 !! i < c2 !! i = LT
            | otherwise = GT

merge :: Counter -> Counter -> Counter
merge counter1 counter2
    | length counter1 /= length counter2 = error "Vectors have different lengths"
    | otherwise = zipWith max counter1 counter2