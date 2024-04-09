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



-- Operation based 2P2P graph
module Graph2P

public export
Vertex : Type
Vertex = Int

public export
Edge : Type
Edge = (Vertex, Vertex)

public export
Graph : Type
Graph = (Vect Vertex, Vect Vertex, Vect Edge, Vect Edge)

public export
initialGraph : Graph
initialGraph = ([], [], [], [])

public export
lookupVertex : Graph -> Vertex -> Bool
lookupVertex (va, vr, _, _) v = elem v va && not (elem v vr)

public export
lookupEdge : Graph -> Edge -> Bool
lookupEdge (va, vr, ea, er) (u, v) =
  elem u va && elem v va && (elem (u, v) (ea ++ er))

public export
addVertex : Graph -> Vertex -> Graph
addVertex (va, vr, ea, er) w = (w :: va, vr, ea, er)

public export
addEdge : Graph -> Edge -> Graph
addEdge (va, vr, ea, er) e@(u, v) =
  if elem u va && elem v va
     then (va, vr, e :: ea, er)
     else (va, vr, ea, er)

public export
removeVertex : Graph -> Vertex -> Graph
removeVertex (va, vr, ea, er) w =
  if elem w va && all (\(u, v) => u /= w && v /= w) (ea ++ er)
     then (removeElem w va, w :: vr, ea, er)
     else (va, vr, ea, er)

public export
removeEdge : Graph -> Edge -> Graph
removeEdge (va, vr, ea, er) e@(u, v) =
  if elem u va && elem v va
     then (va, vr, ea, e :: er)
     else (va, vr, ea, er)


-- Op-based add only monotonic DAG
module MonotonicDAG

%access public export

Vertex : Type
Vertex = Int

Edge : Type
Edge = (Vertex, Vertex)

Graph : Type
Graph = (List Vertex, List Edge)

initialGraph : Graph
initialGraph = ([-1, 1], [(-1, 1)])

lookupVertex : Graph -> Vertex -> Bool
lookupVertex (vs, _) v = elem v vs

lookupEdge : Graph -> Edge -> Bool
lookupEdge (_, es) e = elem e es

pathBetween : Graph -> Vertex -> Vertex -> Bool
pathBetween g w1 wm =
  if w1 == wm
    then True
    else case dropWhile (/= wm) (nub (drop 1 (fst g))) of
           []      => False
           (w::ws) => if lookupEdge g (w1, w)
                        then pathBetween g w wm
                        else False

path : Graph -> Edge -> Bool
path g (u, v) = any (\w1 => w1 == u && any (\wm => wm == v && pathBetween g w1 wm) (fst g)) (fst g)

addEdge : Graph -> Vertex -> Vertex -> Graph
addEdge g u v =
  if lookupVertex g u && lookupVertex g v && path g (u, v)
    then (fst g, (u, v) :: snd g)
    else g

addBetween : Graph -> Vertex -> Vertex -> Vertex -> Graph
addBetween g u v w =
  if lookupVertex g u && lookupVertex g v && lookupVertex g w &&
     path g (u, w) && path g (v, w) && w /= u && w /= v
    then (w :: fst g, (u, w) :: (v, w) :: snd g)
    else g


-- Add remove partial order
module AddRemovePartialOrder

public export
data AddRemovePartialOrder : Type where
  MkAddRemovePartialOrder : (vertices : List Int)
                         -> (removed : List Int)
                         -> (edges : List (Int, Int))
                         -> AddRemovePartialOrder

export
initial : AddRemovePartialOrder
initial = MkAddRemovePartialOrder [-1, 1] [] [(-1, 1)]

export
lookup : AddRemovePartialOrder -> Int -> Bool
lookup (MkAddRemovePartialOrder vertices removed edges) v = elem v vertices

export
before : AddRemovePartialOrder -> Int -> Int -> Bool
before po u v = any (\w => (w == u && lookup po v) ||
                           (w == v && lookup po u) ||
                           (lookup po w && elem (w, u) (edges po) && elem (w, v) (edges po))) (vertices po)
  where
    edges : AddRemovePartialOrder -> List (Int, Int)
    edges (MkAddRemovePartialOrder vertices removed edges) = edges

export
addBetween : AddRemovePartialOrder -> Int -> Int -> Int -> Maybe AddRemovePartialOrder
addBetween po u v w = if not (lookup po w) || not (before po u w) || not (before po w v)
                      then Nothing
                      else Just $ MkAddRemovePartialOrder (w :: vertices po) (removed po) (((u, w), (w, v)) :: edges po)

export
remove : AddRemovePartialOrder -> Int -> Maybe AddRemovePartialOrder
remove po v = if not (lookup po v) || v == -1 || v == 1
              then Nothing
              else Just $ MkAddRemovePartialOrder (filter (/= v) (vertices po))
                                                  (v :: removed po)
                                                  (filter (\(x, y) => x /= v && y /= v) (edges po))



-- Replicable growth array
module RGA

%default total

public export
Vertex : Type
Vertex = (Int, Int)

public export
Edge : Type
Edge = (Vertex, Vertex)

public export
record RGA where
  constructor MkRGA
  va : SortedSet Vertex
  vr : SortedSet Vertex
  edges : SortedMap Vertex (SortedSet Vertex)
  now : IO Int 

public export
empty : IO RGA
empty = do
  t <- primIO $ extern "idris_time_unix" (IO Int)
  pure $ MkRGA
    (singleton (-1, -1))
    (singleton (-1, 0))
    (fromList [((-1, -1), singleton (-1, 0))])
    (const t)

public export
lookup : RGA -> Vertex -> Bool
lookup (MkRGA va vr _) v = isElem v va && not (isElem v vr)

public export
before : RGA -> Vertex -> Vertex -> Bool
before (MkRGA va _ edges) u v =
  lookup (MkRGA va empty edges) u &&
  lookup (MkRGA va empty edges) v &&
  any (\w => (w == u && isElem v va) ||
              (w == v && isElem u va) ||
              (lookup (MkRGA va empty edges) w &&
               isElem u (edges!(w)) &&
               isElem v (edges!(w)))) (toList va)

public export
successor : RGA -> Vertex -> Maybe Vertex
successor rga u =
  if not (lookup rga u)
    then Nothing
    else find (\v => before rga u v && not (before rga v u)) (toList (va rga))

public export
decompose : RGA -> Vertex -> Vertex
decompose _ v = v

public export
addRight : RGA -> Vertex -> Int -> IO RGA
addRight (MkRGA va vr edges now) u a = do
  t <- now
  let w = (a, t)
  if lookup (MkRGA va vr edges now) w
    then pure $ MkRGA va vr edges now
    else pure $ MkRGA
           (insert w va)
           vr
           (insertWith union u (singleton w) edges)
           now

public export
remove : RGA -> Vertex -> Maybe RGA
remove (MkRGA va vr edges now) w =
  if not (lookup (MkRGA va vr edges now) w)
    then Nothing
    else Just $ MkRGA
           (delete w va)
           (insert w vr)
           (mapValues (delete w) edges)
           now


-- Continuous seq
module ContSequence

import Data.Strings

%default total

data ContSeqNode : Type -> Type where
  Leaf : ContSeqNode a
  Node : (left : ContSeqNode a)
      -> (right : ContSeqNode a)
      -> (data : a)
      -> (id : String)
      -> ContSeqNode a

contSeqInsert : Ord a => (value : a) -> (id : String) -> ContSeqNode a -> ContSeqNode a
contSeqInsert value id node@(Node left right data node_id) =
  if id < node_id
    then Node (contSeqInsert value id left) right data node_id
    else Node left (contSeqInsert value id right) data node_id
contSeqInsert value id Leaf = Node Leaf Leaf value id

contSeqLookup : (id : String) -> ContSeqNode a -> Maybe (a, String)
contSeqLookup id (Node left right data node_id) =
  if id == node_id
    then Just (data, node_id)
    else if id < node_id
      then contSeqLookup id left
      else contSeqLookup id right
contSeqLookup id Leaf = Nothing

contSeqAllocateIdentifierBetween : (id1 : String) -> (id2 : String) -> Maybe String
contSeqAllocateIdentifierBetween id1 id2 =
  if length id1 /= 1 || length id2 /= 1
    then Nothing
    else do
      let min_char = min (head id1) (head id2)
      let max_char = max (head id1) (head id2)
      let random_char = Data.Strings.singleton (randomRange min_char max_char)
      pure random_char

contSeqAddBetween : (value : a)
                 -> (id1 : String)
                 -> (id2 : String)
                 -> ContSeqNode a
                 -> ContSeqNode a
contSeqAddBetween value id1 id2 root =
  case root of
    Leaf => case contSeqAllocateIdentifierBetween id1 id2 of
              Just new_id => Node Leaf Leaf value new_id
              Nothing => Leaf
    Node left right data node_id =>
      case contSeqAllocateIdentifierBetween id1 node_id of
        Just new_id =>
          Node (contSeqAddBetween value id1 new_id left) right data node_id
        Nothing =>
          case contSeqAllocateIdentifierBetween node_id id2 of
            Just new_id =>
              Node left (contSeqAddBetween value new_id id2 right) data node_id
            Nothing =>
              root

contSeqRemove : (id : String) -> ContSeqNode a -> ContSeqNode a
contSeqRemove id node = let removeResult = removeNode id node in
                        case removeResult of
                             (True, newNode) => newNode
                             (False, _) => node

removeNode : (id : String) -> ContSeqNode a -> (Bool, ContSeqNode a)
removeNode id node@(Node left right data node_id) =
  if id == node_id
     then removeCurrentNode left right data
     else if id < node_id
             then case removeNode id left of
                       (True, newLeft) => (True, Node newLeft right data node_id)
                       (False, _) => (False, node)
             else case removeNode id right of
                       (True, newRight) => (True, Node left newRight data node_id)
                       (False, _) => (False, node)
removeNode id Leaf = (False, Leaf)

removeCurrentNode : ContSeqNode a -> ContSeqNode a -> a -> (Bool, ContSeqNode a)
removeCurrentNode left right data =
  case left of
       Leaf => case right of
                    Leaf => (True, Leaf)
                    Node _ _ _ _ => let successor = findSuccessor right in
                                     (True, Node Leaf (removeSuccessor successor right) (getData successor) (getId successor))
       Node _ _ _ _ => case right of
                            Leaf => let predecessor = findPredecessor left in
                                     (True, Node (removePredecessor predecessor left) Leaf (getData predecessor) (getId predecessor))
                            Node _ _ _ _ => let successor = findSuccessor right in
                                             (True, Node left (removeSuccessor successor right) (getData successor) (getId successor))

findSuccessor : ContSeqNode a -> ContSeqNode a
findSuccessor (Node left Leaf _ _) = left
findSuccessor (Node left right _ _) = findSuccessor right

removeSuccessor : ContSeqNode a -> ContSeqNode a -> ContSeqNode a
removeSuccessor node@(Node left right data id) tree =
  case left of
       Leaf => tree
       Node _ _ _ _ => Node (removeSuccessor left tree) right data id

getData : ContSeqNode a -> a
getData (Node _ _ data _) = data

getId : ContSeqNode a -> String
getId (Node _ _ _ id) = id

findPredecessor : ContSeqNode a -> ContSeqNode a
findPredecessor (Node left Leaf _ _) = left
findPredecessor (Node left right _ _) = findPredecessor right

removePredecessor : ContSeqNode a -> ContSeqNode a -> ContSeqNode a
removePredecessor node@(Node left right data id) tree =
  case right of
       Leaf => tree
       Node _ _ _ _ => Node left (removePredecessor right tree) data id