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