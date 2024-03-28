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


-- State based PN Counter
module PNCounter

%default total

public export
record PNCounter : (n : Nat) where
    MkPNCounter : Vect n Int -> Vect n Int -> PNCounter n

public export
initialize : (n : Nat) -> Vect n Int -> Vect n Int -> PNCounter n
initialize n p q = MkPNCounter p q

public export
increment : (state : PNCounter n) -> PNCounter n
increment (MkPNCounter p n) = MkPNCounter (updateAt g increment' p) n
    where
        g : Fin n
        g = myID

        increment' : Int -> Int
        increment' x = x + 1

public export
decrement : (state : PNCounter n) -> PNCounter n
decrement (MkPNCounter p n) = MkPNCounter p (updateAt g increment' n)
    where
        g : Fin n
        g = myID

        increment' : Int -> Int
        increment' x = x + 1

public export
value : (state : PNCounter n) -> Int
value (MkPNCounter p n) = sum p - sum n

public export
compare : (state1 : PNCounter n) -> (state2 : PNCounter n) -> Bool
compare (MkPNCounter xp xn) (MkPNCounter yp yn) =
    all (\i => xp ! i <= yp ! i && xn ! i <= yn ! i) (range 0 (length xp))

public export
merge : (state1 : PNCounter n) -> (state2 : PNCounter n) -> PNCounter n
merge (MkPNCounter xp xn) (MkPNCounter yp yn) =
    MkPNCounter (zipWith max xp yp) (zipWith max xn yn)


-- State-based last-writer-wins register
public export
record LastWriterWinsRegister a where
  constructor MkLastWriterWinsRegister
  value     : a
  timestamp : Double

export
create : a -> IO (IORef (LastWriterWinsRegister a))
create initialValue = newIORef (MkLastWriterWinsRegister initialValue 0.0)

export
read : IORef (LastWriterWinsRegister a) -> IO a
read ref = do
  MkLastWriterWinsRegister v _ <- readIORef ref
  pure v

export
write : IORef (LastWriterWinsRegister a) -> a -> Double -> IO ()
write ref newValue newTimestamp = atomicModifyIORef ref $ \(MkLastWriterWinsRegister v t) =>
  if newTimestamp > t
     then (MkLastWriterWinsRegister newValue newTimestamp, ())
     else (MkLastWriterWinsRegister v t, ())

export
compareAndSwap : IORef (LastWriterWinsRegister a) -> a -> Double -> a -> Double -> IO Bool
compareAndSwap ref expectedValue expectedTimestamp newValue newTimestamp =
  atomicModifyIORef ref $ \(MkLastWriterWinsRegister v t) =>
    if v == expectedValue && t == expectedTimestamp
       then (MkLastWriterWinsRegister newValue newTimestamp, True)
       else (MkLastWriterWinsRegister v t, False)



-- Operation-based last-writer-wins register
module OpBasedLWWRegister

import Data.IORef

public export
record Value where
  constructor MkValue
  val : Int
  ts  : Double

public export
data Op = UpdateOp Value | Reset

public export
record Register where
  constructor MkRegister
  value   : Value
  pending : List Op

export
create : Int -> IO (IORef Register)
create initialValue = newIORef (MkRegister (MkValue initialValue 0.0) [])

export
read : IORef Register -> IO Int
read ref = do
  MkRegister (MkValue v _) _ <- readIORef ref
  pure v

export
update : IORef Register -> Int -> Double -> IO ()
update ref newValue newTimestamp = atomicModifyIORef ref $ \(MkRegister (MkValue v t) pending) =>
  if newTimestamp > t
     then (MkRegister (MkValue newValue newTimestamp) [], ())
     else (MkRegister (MkValue v t) (UpdateOp (MkValue newValue newTimestamp) :: pending), ())

export
reset : IORef Register -> IO ()
reset ref = atomicModifyIORef ref $ \(MkRegister v pending) =>
  (MkRegister v (Reset :: pending), ())

applyPending : Register -> Register
applyPending (MkRegister v []) = MkRegister v []
applyPending (MkRegister v (UpdateOp newV : ops)) =
  let MkRegister v' ops' = applyPending (MkRegister newV ops)
  in applyPending (MkRegister (max v' newV) ops')
applyPending (MkRegister _ (Reset : ops)) =
  applyPending (MkRegister (MkValue 0 0.0) ops)

export
downstream : IORef Register -> IO Register
downstream ref = do
  reg <- readIORef ref
  writeIORef ref (applyPending reg)
  readIORef ref