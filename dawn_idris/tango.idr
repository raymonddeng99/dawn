module AuxTree

public export
data AuxTree : Type -> Type where
  Empty : AuxTree a
  Node : AuxTree a -> a -> AuxTree a -> AuxTree a

public export
empty : AuxTree a
empty = Empty

public export
insert : Ord a => a -> AuxTree a -> AuxTree a
insert x Empty = Node Empty x Empty
insert x (Node left y right) =
  case compare x y of
    LT => Node (insert x left) y right
    EQ => Node left y right
    GT => Node left y (insert x right)

public export
find : Ord a => a -> AuxTree a -> Bool
find x Empty = False
find x (Node left y right) =
  case compare x y of
    LT => find x left
    EQ => True
    GT => find x right

module TangoTree

import AuxTree

public export
TangoTree : Type -> Type
TangoTree a = List (AuxTree a)

public export
empty : TangoTree a
empty = [empty]

public export
find : Ord a => a -> TangoTree a -> Bool
find x = any (find x)

public export
split : Ord a => a -> AuxTree a -> (AuxTree a, AuxTree a)
split x Empty = (Empty, Empty)
split x (Node left y right) =
  case compare x y of
    LT => let (l1, l2) = split x left in (l1, Node l2 y right)
    EQ => (Node left y Empty, Empty)
    GT => let (r1, r2) = split x right in (Node left y r1, r2)

public export
join : AuxTree a -> AuxTree a -> AuxTree a
join Empty right = right
join left Empty = left
join (Node ll lx lr) (Node rl ry rr) =
  case compare lx ry of
    LT => Node ll lx (join lr (Node rl ry rr))
    EQ => Node ll lx (join lr rr)
    GT => Node (join (Node ll lx lr) rl) ry rr

public export
update : Ord a => a -> TangoTree a -> TangoTree a
update x [] = [Node Empty x Empty]
update x (aux :: rest) =
  let (left, right) = split x aux in
      join left (Node Empty x Empty) :: rest