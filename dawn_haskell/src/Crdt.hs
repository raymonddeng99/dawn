-- (* CRDTs *)

{-
Specification: CRDTs = state-based | op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
-}


import Data.List (foldl', nub)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
import Data.Array (Array, (!), (//), accumArray, elems, listArray, range)

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

-- State-based PN Counter
module PNCounter (
    State,
    initialize,
    increment,
    decrement,
    value,
    compare,
    merge
) where

type Payload = [Int]
data State = State { p :: Payload, n :: Payload }

initialize :: Int -> Payload -> Payload -> State
initialize n p n = State (copy p) (copy n)
  where
    copy xs = replicate n 0

increment :: State -> State
increment (State p n) = State (update p) n
  where
    update xs = xs // [(g, xs !! g + 1)]
    g = myID

decrement :: State -> State
decrement (State p n) = State p (update n)
  where
    update xs = xs // [(g, xs !! g + 1)]
    g = myID

value :: State -> Int
value (State p n) = sum p - sum n

compare :: State -> State -> Bool
compare (State xp xn) (State yp yn) =
    and [xp !! i <= yp !! i && xn !! i <= yn !! i | i <- [0 .. length xp - 1]]

merge :: State -> State -> State
merge (State xp xn) (State yp yn) = State (mergePayload xp yp) (mergePayload xn yn)
  where
    mergePayload xs ys = zipWith max xs ys


-- State-based last-writer-wins register
module LastWriterWinsRegister (Register, create, read, write, compareAndSwap) where

data Register a = Register { value :: a, timestamp :: Float }

create :: a -> IO (IORef (Register a))
create initialValue = newIORef (Register initialValue 0.0)

read :: IORef (Register a) -> IO a
read ref = do
  Register v _ <- readIORef ref
  return v

write :: IORef (Register a) -> a -> Float -> IO ()
write ref newValue newTimestamp = atomicModifyIORef ref $ \(Register v t) ->
  if newTimestamp > t
    then (Register newValue newTimestamp, ())
    else (Register v t, ())

compareAndSwap :: IORef (Register a) -> a -> Float -> a -> Float -> IO Bool
compareAndSwap ref expectedValue expectedTimestamp newValue newTimestamp =
  atomicModifyIORef ref $ \(Register v t) ->
    if v == expectedValue && t == expectedTimestamp
      then (Register newValue newTimestamp, True)
      else (Register v t, False)


-- Op-based last-writer-wins register
module OpBasedLWWRegister (Register, create, read, update, reset, downstream) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)

data Value = Value Int Float
  deriving (Eq, Show)

data Op = Update Value | Reset
  deriving (Eq, Show)

data Register = Register { value :: Value, pending :: [Op] }
  deriving (Eq, Show)

create :: Int -> IO (IORef Register)
create initialValue = newIORef (Register (Value initialValue 0.0) [])

read :: IORef Register -> IO Int
read ref = do
  Register (Value v _) _ <- readIORef ref
  return v

update :: IORef Register -> Int -> Float -> IO ()
update ref newValue newTimestamp = atomicModifyIORef ref $ \(Register (Value v t) pending) ->
  if newTimestamp > t
    then (Register (Value newValue newTimestamp) [], ())
    else (Register (Value v t) (Update (Value newValue newTimestamp) : pending), ())

reset :: IORef Register -> IO ()
reset ref = atomicModifyIORef ref $ \(Register v pending) ->
  (Register v (Reset : pending), ())

applyPending :: Register -> Register
applyPending (Register v []) = Register v []
applyPending (Register v (Update newV : ops)) =
  let Register v' ops' = applyPending (Register newV ops)
  in applyPending (Register (max v' newV) ops')
applyPending (Register _ (Reset : ops)) =
  applyPending (Register (Value 0 0.0) ops)

downstream :: IORef Register -> IO Register
downstream ref = do
  reg <- readIORef ref
  writeIORef ref (applyPending reg)
  readIORef ref

-- State-based multi-value register
module MVRegister (
    MVRegister,
    initial,
    queryIncrementVV,
    updateAssign,
    queryValue,
    compare,
    merge
) where

type X = Int
type Version = Array Int Int

type Payload = [(X, Version)]

initial :: Payload
initial = [(-1, listArray (0, 0) [])]

queryIncrementVV :: IO Version
queryIncrementVV = do
    g <- myID
    let vs = map snd initial
        f xs = foldl' max 0 xs + 1
        v' = accumArray f 0 (range (0, length vs - 1)) . map (map f) $ vs
    return $ v' // [(g, v' ! g + 1)]

updateAssign :: [X] -> IO Payload
updateAssign set_r = do
    v <- queryIncrementVV
    return $ map (,v) set_r ++ initial

queryValue :: Payload
queryValue = initial

compare :: Payload -> Payload -> Bool
compare a b = any (\(x, v) -> any (\(x', v') -> any (< v) v') b) a

merge :: Payload -> Payload -> Payload
merge a b =
    let a' = filter (\(x, v) -> any (\(y, w) -> any (>= v ! (length v - 1)) w || any (< w) v) b) a
        b' = filter (\(y, w) -> any (\(x, v) -> any (>= w ! (length w - 1)) v || any (< v) w) a) b
    in nub $ a' ++ b'


-- State-based grow-only set
module GSet (P) where

data GSet P = GSet { data :: Set (P.t) }

empty :: GSet P
empty = GSet mempty

add :: GSet P -> P.t -> GSet P
add s e = s { data = data s `Set.insert` e }

lookup :: GSet P -> P.t -> Bool
lookup s e = Set.member e (data s)

compare :: GSet P -> GSet P -> Bool
compare s1 s2 = data s1 == data s2

merge :: GSet P -> GSet P -> GSet P
merge s1 s2 = GSet $ Set.union (data s1) (data s2)