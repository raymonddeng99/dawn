module PriorityQueue

import Data.Vect

%default total

public export
record PriorityQueue a where
  constructor MkPQ
  size : Nat
  root : Cluster a

public export
data Cluster : Type -> Type where
  Leaf : a -> Cluster a
  Node : Vect 2 (Cluster a) -> Cluster a

export
clusterSize : Nat
clusterSize = 2

export
empty : PriorityQueue a
empty = MkPQ 0 (Leaf _)

export
isEmpty : PriorityQueue a -> Bool
isEmpty (MkPQ 0 _) = True
isEmpty _          = False

export
insert : Ord a => a -> PriorityQueue a -> PriorityQueue a
insert x (MkPQ sz (Leaf _)) = MkPQ (S sz) (Leaf x)
insert x (MkPQ sz (Node cs)) = MkPQ (S sz) (insertHelper x cs)

insertHelper : Ord a => a -> Vect 2 (Cluster a) -> Cluster a
insertHelper x [Leaf y, c] with (x `compare` y)
  | GT = Node [Leaf y, insertHelper x c]
  | _  = Node [Leaf x, c]
insertHelper x [Leaf y, Leaf z] with (x `compare` y)
  | GT = Node [Leaf y, Leaf x]
  | _  = Node [Leaf x, Leaf y]
