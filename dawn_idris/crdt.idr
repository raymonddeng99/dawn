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


-- State based increment-only counter
module GCounter

public export
data GCounter : Nat -> Type where
  MkGCounter : (len : Nat) -> Vect len Nat -> GCounter len

public export
create : (size : Nat) -> GCounter size
create size = MkGCounter size $ replicate size 0

public export
update : (c : GCounter n) -> (i : Nat) -> { auto prf : LTB i n = True } -> GCounter n
update (MkGCounter len vec) i { prf } = MkGCounter len $ updateAt i (vec ! i + 1) vec

public export
query : (c : GCounter n) -> (i : Nat) -> { auto prf : LTB i n = True } -> Nat
query (MkGCounter len vec) i { prf } = vec ! i

public export
compare : (c1 : GCounter n) -> (c2 : GCounter n) -> Ordering
compare (MkGCounter len vec1) (MkGCounter _ vec2) =
  let go : Nat -> Ordering
      go i = if i >= len
               then EQ
               else case comparing (vec1 ! i) (vec2 ! i) of
                         EQ => go (i + 1)
                         other => other
  in go 0

public export
merge : (c1 : GCounter n) -> (c2 : GCounter n) -> GCounter n
merge (MkGCounter len vec1) (MkGCounter _ vec2) =
  MkGCounter len $ zipWith max vec1 vec2