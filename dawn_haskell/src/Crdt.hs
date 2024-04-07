-- (* CRDTs *)

{-
Specification: CRDTs = state-based | op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
-}


import Data.List (foldl', nub)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
import Data.Array (Array, (!), (//), accumArray, elems, listArray, range)
import Data.Set (Set)
import qualified Data.Map as Map

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


-- State-based 2P set
module StateBased2PSet(
  StateBased2PSet,
  empty,
  lookup,
  add,
  remove,
  compare,
  merge
) where

data StateBased2PSet a = S2PSet { added :: Set.Set a, removed :: Set.Set a }

empty :: StateBased2PSet a
empty = S2PSet Set.empty Set.empty

lookup :: Ord a => StateBased2PSet a -> a -> Bool
lookup (S2PSet a r) e = Set.member e a && not (Set.member e r)

add :: Ord a => StateBased2PSet a -> a -> StateBased2PSet a
add (S2PSet a r) e = S2PSet (Set.insert e a) r

remove :: Ord a => StateBased2PSet a -> a -> StateBased2PSet a
remove (S2PSet a r) e
  | lookup (S2PSet a r) e = S2PSet a (Set.insert e r)
  | otherwise = S2PSet a r

compare :: Ord a => StateBased2PSet a -> StateBased2PSet a -> Bool
compare (S2PSet a1 r1) (S2PSet a2 r2) =
  Set.isSubsetOf a1 a2 && Set.isSubsetOf r1 r2

merge :: Ord a => StateBased2PSet a -> StateBased2PSet a -> StateBased2PSet a
merge (S2PSet a1 r1) (S2PSet a2 r2) =
  S2PSet (Set.union a1 a2) (Set.union r1 r2)



-- Op based 2p set with unique elements
module USet (
    USet,
    emptyUSet,
    lookup,
    add,
    remove
) where

type Element = Int

newtype USet = USet (Set.Set Element)
  deriving (Eq, Show)

emptyUSet :: USet
emptyUSet = USet Set.empty

lookup :: Element -> USet -> Bool
lookup e (USet s) = Set.member e s

add :: Element -> USet -> USet
add e (USet s)
  | lookup e (USet s) = USet s
  | otherwise = USet (Set.insert e s)

remove :: Element -> USet -> USet
remove e (USet s)
  | lookup e (USet s) = USet (Set.delete e s)
  | otherwise = USet s


-- Molli, Weiss, Skaf set
module MWSSet (
    MWSSet,
    emptyMWSSet,
    lookup,
    add,
    remove
) where

type Element = Int
type MWSSet = Map.Map Element Int

emptyMWSSet :: MWSSet
emptyMWSSet = Map.empty

lookup :: Element -> MWSSet -> Bool
lookup e s = case Map.lookup e s of
    Just k -> k > 0
    Nothing -> False

add :: Element -> MWSSet -> MWSSet
add e s =
    let j = case Map.lookup e s of
                Just k | k < 0 -> abs k + 1
                _ -> 1
    in Map.insert e j s

remove :: Element -> MWSSet -> MWSSet
remove e s =
    case Map.lookup e s of
        Just k' -> Map.insert e (k' - 1) (Map.delete e s)
        Nothing -> s


-- Operation based observed-remove set
module ORSet (
  UniqueTag,
  Elem,
  Set,
  uniqueTag,
  uniqueElements,
  emptySet,
  addORSet,
  lookupORSet,
  removeORSet,
  downstreamORSet,
  preConditionORSet,
  atSourceORSet
) where

type UniqueTag = ()
type Elem = (Int, UniqueTag)
type Set = [Elem]

uniqueTag :: UniqueTag
uniqueTag = ()

uniqueElements :: Set -> Set
uniqueElements = unique
  where
    unique [] = []
    unique (x:xs) = x : unique (filter (/= x) xs)

emptySet :: Set
emptySet = []

addORSet :: Int -> Set -> Set
addORSet e set = uniqueElements ((e, uniqueTag) : set)

lookupORSet :: Int -> Set -> Bool
lookupORSet e = any (\(x, _) -> x == e)

removeORSet :: Int -> Set -> Set
removeORSet e = filter (\(x, _) -> x /= e)

downstreamORSet :: Set -> Set
downstreamORSet = id

preConditionORSet :: (Set -> Set) -> Set -> Set
preConditionORSet f = f

atSourceORSet :: Int -> Int
atSourceORSet = id


-- Operation based 2P2P graph
module Graph2P
  ( Vertex
  , Edge
  , Graph
  , initialGraph
  , lookupVertex
  , lookupEdge
  , addVertex
  , addEdge
  , removeVertex
  , removeEdge
  ) where

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = (Set.Set Vertex, Set.Set Vertex, Set.Set Edge, Set.Set Edge)

initialGraph :: Graph
initialGraph = (Set.empty, Set.empty, Set.empty, Set.empty)

lookupVertex :: Graph -> Vertex -> Bool
lookupVertex (va, vr, _, _) v = Set.member v (Set.difference va vr)

lookupEdge :: Graph -> Edge -> Bool
lookupEdge (va, vr, ea, er) (u, v) =
  Set.member u va && Set.member v va && Set.member (u, v) (Set.union ea er)

addVertex :: Graph -> Vertex -> Graph
addVertex (va, vr, ea, er) w = (Set.insert w va, vr, ea, er)

addEdge :: Graph -> Vertex -> Vertex -> Graph
addEdge (va, vr, ea, er) u v
  | Set.member u va && Set.member v va = (va, vr, Set.insert (u, v) ea, er)
  | otherwise = (va, vr, ea, er)

removeVertex :: Graph -> Vertex -> Graph
removeVertex (va, vr, ea, er) w
  | Set.member w va && all (\(u, v) -> u /= w && v /= w) (Set.union ea er) =
    (Set.delete w va, Set.insert w vr, ea, er)
  | otherwise = (va, vr, ea, er)

removeEdge :: Graph -> Edge -> Graph
removeEdge (va, vr, ea, er) (u, v)
  | Set.member u va && Set.member v va = (va, vr, ea, Set.insert (u, v) er)
  | otherwise = (va, vr, ea, er)

-- Op-based add-only monotonic DAG
module MonotonicDAG
  ( Vertex, Edge, Graph
  , initialGraph
  , lookupVertex, lookupEdge, path
  , addEdge, addBetween
  ) where

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = ([Vertex], [Edge])

initialGraph :: Graph
initialGraph = ([-1, 1], [(-1, 1)])

lookupVertex :: Graph -> Vertex -> Bool
lookupVertex (vs, _) v = v `elem` vs

lookupEdge :: Graph -> Edge -> Bool
lookupEdge (_, es) e = e `elem` es

path :: Graph -> Edge -> Bool
path g@(vs, es) (u, v) =
  any (\w1 -> w1 == u && any (\wm -> wm == v && pathBetween w1 wm) vs) vs
  where
    pathBetween w1 wm
      | w1 == wm = True
      | otherwise = case span (/= wm) (drop 1 vs) of
                      (ws, w:_) -> (w1, w) `elem` es && pathBetween w wm
                      _         -> False

addEdge :: Graph -> Vertex -> Vertex -> Graph
addEdge g@(vs, es) u v
  | lookupVertex g u && lookupVertex g v && path g (u, v) = (vs, (u, v) : es)
  | otherwise = g

addBetween :: Graph -> Vertex -> Vertex -> Vertex -> Graph
addBetween g@(vs, es) u v w
  | lookupVertex g u && lookupVertex g v && lookupVertex g w &&
    path g (u, w) && path g (v, w) && w /= u && w /= v =
    (w:vs, (u, w) : (v, w) : es)
  | otherwise = g


-- Add remove partial order
module AddRemovePartialOrder (
    Vertex,
    Edge,
    PartialOrder(..),
    initial,
    lookup,
    before,
    addBetween,
    remove
) where

type Vertex = Int
type Edge = (Vertex, Vertex)

data PartialOrder = PartialOrder
    { vertices :: [Vertex]
    , removed :: [Vertex]
    , edges :: [Edge]
    }

initial :: PartialOrder
initial = PartialOrder
    { vertices = [-1, 1]
    , removed = []
    , edges = [(-1, 1)]
    }

lookup :: PartialOrder -> Vertex -> Bool
lookup po v = v `elem` vertices po

before :: PartialOrder -> Vertex -> Vertex -> Bool
before po u v = any isBetween (vertices po)
  where
    isBetween w = (u == w && v `elem` vertices po) ||
                  (v == w && u `elem` vertices po) ||
                  (lookup po w && (u, w) `elem` edges po && (w, v) `elem` edges po)

addBetween :: PartialOrder -> Vertex -> Vertex -> Vertex -> PartialOrder
addBetween po u v w
    | not (lookup po w) || not (before po u w) || not (before po w v) = error "addBetween precondition violated"
    | otherwise = po { vertices = w : vertices po, edges = (u, w) : (w, v) : edges po }

remove :: PartialOrder -> Vertex -> PartialOrder
remove po v
    | not (lookup po v) || v == -1 || v == 1 = error "remove precondition violated"
    | otherwise = po { vertices = filter (/= v) (vertices po)
                     , removed = v : removed po
                     , edges = filter (\(x, y) -> x /= v && y /= v) (edges po)
                     }

-- Replicable growth array
module RGA (
    Vertex,
    Edge,
    RGA(..),
    empty,
    lookup,
    before,
    successor,
    decompose,
    addRight,
    remove
) where

type Vertex = (Int, Int)
type Edge = (Vertex, Vertex)

data RGA = RGA {
    va :: [Vertex],
    vr :: [Vertex],
    edges :: [Edge],
    now :: IO Int
}

empty :: IO Int -> IO RGA
empty nowFn = do
    t <- nowFn
    return $ RGA [((-1), (-1))] [((-1), 0)] [] nowFn

lookup :: RGA -> Vertex -> Bool
lookup rga v = v `elem` va rga && not (v `elem` vr rga)

before :: RGA -> Vertex -> Vertex -> Bool
before rga u v = lookup rga u && lookup rga v && any isBetween (va rga)
  where
    isBetween w = (u == w && v `elem` va rga) ||
                  (v == w && u `elem` va rga) ||
                  (lookup rga w && (u, w) `elem` edges rga && (w, v) `elem` edges rga)

successor :: RGA -> Vertex -> Vertex
successor rga u
    | not (lookup rga u) = error "Vertex not found"
    | otherwise = head $ dropWhile (`before` rga u) $ cycle $ va rga

decompose :: RGA -> Vertex -> Vertex
decompose _ v = v

addRight :: RGA -> Vertex -> Int -> IO RGA
addRight rga u a = do
    t <- now rga
    let w = (a, t)
    if lookup rga w
        then error "Timestamp conflict"
        else return $ rga { va = w : va rga,
                            edges = (u, w) : filter (\(v, w') -> v /= u || w' /= w) (edges rga) }

remove :: RGA -> Vertex -> RGA
remove rga w
    | not (lookup rga w) = error "Vertex not found"
    | otherwise = rga { vr = w : vr rga }