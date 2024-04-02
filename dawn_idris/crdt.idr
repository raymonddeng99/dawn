-- CRDTs

{-|
   Specification: CRDTs = state-based | op-based

   State-based require states form monotonic lattice and merge computes LUB (least upper bound)
   Op-based require delivery order exists and concurrent updates commute
-}

import Data.Vect
import Data.SortedSet
import Data.SortedMap

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


-- State-based multi-value register
module MVRegister

public export
data X = MkX Int

public export
Version : Type
Version = List Int

public export
Payload : Type
Payload = List (X, Version)

export
initial : Payload
initial = [(MkX (-1), [])]

export
queryIncrementVV : (Int -> IO Int) -> Payload -> IO Version
queryIncrementVV myID payload = do
  g <- myID
  let vs = map snd payload
      maxVersion = foldr max 0 $ concatMap id vs
  let newVersion = replicate (length payload) (maxVersion + 1)
  pure $ updateAt g (+ 1) newVersion

export
updateAssign : (Int -> IO Int) -> List X -> Payload -> IO Payload
updateAssign myID set_r payload = do
  g <- myID
  newVersion <- queryIncrementVV myID payload
  pure $ map (\x => (x, newVersion)) set_r ++ payload

export
queryValue : Payload -> Payload
queryValue = id

export
compare : Payload -> Payload -> Bool
compare a b = any (\(x, v) => any (\(x', v') => any (> v') v) (filter (\(x'', _) => x == x'') b)) a

export
merge : Payload -> Payload -> Payload
merge a b =
  let a' = filter (\(x, v) => any (\(y, w) => (last w >= last v) || any (< w) v) b) a
      b' = filter (\(y, w) => any (\(x, v) => (last v >= last w) || any (< v) w) a) b
  in nubBy (\(x, _), (y, _) => x == y) (a' ++ b')


-- State-based grow-only set
module GSet (P : Type) where

data GSet P = GSet { data : Set P }

empty : GSet P
empty = GSet mempty

add : GSet P -> P -> GSet P
add s e = GSet { data = data s `Set.insert` e }

lookup : GSet P -> P -> Bool
lookup s e = Set.member e (data s)

compare : GSet P -> GSet P -> Bool
compare s1 s2 = data s1 == data s2

merge : GSet P -> GSet P -> GSet P
merge s1 s2 = GSet { data = Set.union (data s1) (data s2) }


module StateBased2PSet

public export
record StateBased2PSet a where
  constructor MkSet
  added : SortedSet a
  removed : SortedSet a

public export
empty : StateBased2PSet a
empty = MkSet empty empty

public export
lookup : Ord a => StateBased2PSet a -> a -> Bool
lookup (MkSet added removed) e = elem e added && not (elem e removed)

public export
add : Ord a => StateBased2PSet a -> a -> StateBased2PSet a
add (MkSet added removed) e =
  if lookup (MkSet added removed) e
    then MkSet added removed
    else MkSet (insert e added) removed

public export
remove : Ord a => StateBased2PSet a -> a -> StateBased2PSet a
remove (MkSet added removed) e =
  if lookup (MkSet added removed) e
    then MkSet added (insert e removed)
    else MkSet added removed

public export
compare : Ord a => StateBased2PSet a -> StateBased2PSet a -> Bool
compare (MkSet a1 r1) (MkSet a2 r2) =
  isSubsetOf a1 a2 && isSubsetOf r1 r2

public export
merge : Ord a => StateBased2PSet a -> StateBased2PSet a -> StateBased2PSet a
merge (MkSet a1 r1) (MkSet a2 r2) =
  MkSet (union a1 a2) (union r1 r2)


-- Op based 2p set with unique elements
module USet

Element : Type
Element = Int

USet : Type
USet = Vect Element

emptyUSet : USet
emptyUSet = []

lookup : (e : Element) -> (uset : USet) -> Bool
lookup e uset = elem e uset

add : (e : Element) -> (uset : USet) -> USet
add e uset = if lookup e uset then uset else e :: uset

remove : (e : Element) -> (uset : USet) -> USet
remove e uset = filter (/= e) uset


-- Molli, Weiss, Skaf set
MWSSet : Type -> Type
MWSSet a = SortedMap a Int

emptyMWSSet : Ord a => MWSSet a
emptyMWSSet = empty

lookup : Ord a => a -> MWSSet a -> Bool
lookup e s = case lookup e s of
                Just k => k > 0
                Nothing => False

add : Ord a => a -> MWSSet a -> MWSSet a
add e s =
    let j = case lookup e s of
                Just k | k < 0 => abs k + 1
                _ => 1
    in insert e j s

remove : Ord a => a -> MWSSet a -> MWSSet a
remove e s =
    case lookup e s of
        Just k' => let s' = delete e s in
                       if k' > 1
                          then insert e (k' - 1) s'
                          else s'
        Nothing => s


-- Operation based observed-remove set
module ORSet (P : Type) where

public export
record ORSet a where
  constructor MkORSet
  added    : SortedSet a
  removed  : SortedSet a

public export
empty : ORSet a
empty = MkORSet empty empty

public export
lookup : Ord a => a -> ORSet a -> Bool
lookup x (MkORSet added removed) = x `elem` added && not (x `elem` removed)

public export
add : Ord a => a -> ORSet a -> ORSet a
add x (MkORSet added removed) = if lookup x (MkORSet added removed)
                                   then MkORSet added removed
                                   else MkORSet (insert x added) removed

public export
remove : Ord a => a -> ORSet a -> ORSet a
remove x (MkORSet added removed) = if lookup x (MkORSet added removed)
                                      then MkORSet added (insert x removed)
                                      else MkORSet added removed

public export
compare : Ord a => ORSet a -> ORSet a -> Bool
compare (MkORSet added1 removed1) (MkORSet added2 removed2) =
  added1 == added2 && removed1 == removed2

public export
merge : Ord a => ORSet a -> ORSet a -> ORSet a
merge (MkORSet added1 removed1) (MkORSet added2 removed2) =
  MkORSet (union added1 added2) (union removed1 removed2)