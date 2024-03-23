-- CRDTs

{-|
   Specification: CRDTs = state-based | op-based

   State-based require states form monotonic lattice and merge computes LUB (least upper bound)
   Op-based require delivery order exists and concurrent updates commute
-}

import Data.Vect

data Operation = Increment | Decrement

Counter : Type
Counter = (Int, Vect n Operation)

initial : Counter
initial = (0, [])

apply : Counter -> Counter
apply (value, ops) = (foldl (\v, op => case op of
                                           Increment => v + 1
                                           Decrement => v - 1) value ops, [])

merge : Counter -> Counter -> Counter
merge (value1, ops1) (value2, ops2) =
    let mergedOps = ops1 ++ ops2
        finalValue = foldl (\v, op => case op of
                                           Increment => v + 1
                                           Decrement => v - 1) (max value1 value2) mergedOps
    in (finalValue, [])

downstream : Vect n Operation
downstream = []

update : Operation -> Counter -> Counter
update op (value, ops) = (value, op :: ops)