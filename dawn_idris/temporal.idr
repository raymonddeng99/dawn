import Data.Vect

%default total

data Node : Type -> Type where
  One : a -> Node a
  Two : a -> a -> Node a
  Three : a -> a -> a -> Node a
  Four : a -> a -> a -> a -> Node a

record Deque a where
  constructor MkDeque
  front : List (Node a)
  back : List (Node a)

empty : Deque a
empty = MkDeque [] []

isEmpty : Deque a -> Bool
isEmpty (MkDeque [] []) = True
isEmpty _ = False

addFront : a -> Deque a -> Deque a
addFront x (MkDeque front back) =
  let revNodes : List (Node a) -> List (Node a)
      revNodes [] = [One x]
      revNodes (One y :: xs) =
        Four y y y y :: revNodes xs ++ [One x]
      revNodes (Two y z :: xs) =
        Three y y z :: Four z z z z :: revNodes xs ++ [One x]
      revNodes (Three y z w :: xs) =
        Two y z :: Two z w :: Four w w w w :: revNodes xs ++ [One x]
      revNodes (Four y z w v :: xs) =
        One y :: One z :: One w :: One v :: revNodes xs ++ [One x]
  in MkDeque (revNodes front) []

addBack : a -> Deque a -> Deque a
addBack x (MkDeque front back) =
  let revNodes : List (Node a) -> List (Node a)
      revNodes [] = [One x]
      revNodes (One y :: xs) =
        One y :: revNodes xs ++ [Four x x x x]
      revNodes (Two y z :: xs) =
        Three y z z :: Four y y y x :: revNodes xs
      revNodes (Three y z w :: xs) =
        Two y z :: Two w w :: Four z z z z :: revNodes xs ++ [One x]
      revNodes (Four y z w v :: xs) =
        One y :: One z :: One w :: One v :: revNodes xs ++ [One x]
  in MkDeque front (revNodes back)

takeFront : Deque a -> Maybe (a, Deque a)
takeFront (MkDeque [] back) = Nothing
takeFront (MkDeque (One x :: front') back) =
  let back' = case back of
                 [] => []
                 [One x] => []
                 nodes => reverse nodes
  in Just (x, MkDeque front' back')
takeFront (MkDeque (Two x y :: front') back) =
  let back' = case back of
                 [] => []
                 [Two x y] => [One x, One y]
                 One x :: nodes => reverse (Two x y :: nodes)
                 _ => reverse back
  in Just (x, MkDeque (One y :: front') back')
takeFront (MkDeque (Three x y z :: front') back) =
  let back' = case back of
                 [] => []
                 [Three x y z] => [One x, One y, One z]
                 One x :: nodes => reverse (Three x y z :: nodes)
                 Two x y :: nodes => reverse (Two x y :: Two y z :: nodes)
                 _ => reverse back
  in Just (x, MkDeque (Two y z :: front') back')
takeFront (MkDeque (Four w x y z :: front') back) =
  let back' = case back of
                 [] => []
                 [Four w x y z] => [One w, One x, One y, One z]
                 One x :: nodes => reverse (Four w x y z :: nodes)
                 Two x y :: nodes => reverse (Two x y :: Two y z :: Two z w :: nodes)
                 Three x y z :: nodes => reverse (Three x y z :: Two z w :: One w :: nodes)
                 _ => reverse back
  in Just (w, MkDeque (Three x y z :: front') back')

takeBack : Deque a -> Maybe (a, Deque a)
takeBack (MkDeque front []) = Nothing
takeBack (MkDeque front (One x :: back')) =
  let front' = case reverse front of
                    [] => []
                    One x :: front'' => reverse front''
                    Two x y :: front'' => reverse (One y :: front'')
                    Three x y z :: front'' => reverse (Two y z :: front'')
                    Four w x y z :: front'' => reverse (Three x y z :: front'')
  in Just (x, MkDeque front' back')
takeBack (MkDeque front (Two x y :: back')) =
  let front' = case reverse front of
                    [] => []
                    One x :: front'' => reverse (Two y x :: front'')
                    Two x z :: front'' => reverse (Three z y x :: front'')
                    Three w x z :: front'' => reverse (Four z x w y :: front'')
                    Four w x y z :: front'' => reverse (Three z y x :: Two w y :: One w :: front'')
  in Just (y, MkDeque front' back')
takeBack (MkDeque front (Three x y z :: back')) =
  let front' = case reverse front of
                    [] => []
                    One x :: front'' => reverse (Three z y x :: front'')
                    Two x w :: front'' => reverse (Four w z y x :: front'')
                    Three w x y :: front'' => reverse (Two y x :: Two w z :: One w :: front'')
                    Four w x y z :: front'' => reverse (One z :: Three y x w :: front'')
  in Just (z, MkDeque front' back')
takeBack (MkDeque front (Four w x y z :: back')) =
  let front' = case reverse front of
                    [] => []
                    One x :: front'' => reverse (Four z y x w :: front'')
                    Two x y :: front'' => reverse (Two z y :: Two y x :: One w :: front'')
                    Three x y z :: front'' => reverse (One z :: Two y x :: One w :: front'')
                    Four w' x' y' z' :: front'' => reverse (Three z' y' x' :: front'')
  in Just (z, MkDeque front' back')


-- Partially retroactive priority queue
module PriorityQueue

import Data.Nat
import Data.So

%default total

data PQueue : Type -> Type where
  Nil : PQueue a
  (::) : (Int, a) -> PQueue a -> PQueue a

addTime : PQueue a -> PQueue a
addTime Nil = Nil
addTime ((t, x) :: xs) = (S t, x) :: addTime xs

insert : Ord a => a -> PQueue a -> PQueue a
insert x q = addTime ((0, x) :: q)

isEmpty : PQueue a -> Bool
isEmpty Nil = True
isEmpty _   = False

findMin : Ord a => PQueue a -> Maybe a
findMin Nil                 = Nothing
findMin ((t, x) :: xs)      = case getMin xs of
                                   Nothing => Just x
                                   Just (t', x') => if t < t' then Just x else Just x'
  where
    getMin : PQueue a -> Maybe (Int, a)
    getMin Nil = Nothing
    getMin ((t, x) :: xs) = case getMin xs of
                                 Nothing => Just (t, x)
                                 Just (t', x') => if t < t' then Just (t, x) else Just (t', x')

deleteMin : Ord a => PQueue a -> (Maybe a, PQueue a)
deleteMin Nil = (Nothing, Nil)
deleteMin q  = case findMin q of
                    Nothing => (Nothing, Nil)
                    Just x  => (Just x, delete x q)
  where
    delete : Ord a => a -> PQueue a -> PQueue a
    delete x Nil = Nil
    delete x ((t, y) :: ys) = if x == y then delete x ys else (t, y) :: delete x ys

retroactiveUpdate : Ord a => PQueue a -> Int -> a -> PQueue a
retroactiveUpdate Nil _ _ = Nil
retroactiveUpdate ((t, x) :: xs) t' x' = updateElem t t' x' :: retroactiveUpdate xs t' x'
  where
    updateElem : Int -> Int -> a -> (Int, a)
    updateElem t t' x' = if t <= t' then (t, x') else (t, x)



-- Simple Confluently Persistent Catenable Lists, Tarjan et al
module PersistentList

public export
data PersistentList : Type -> Type where
  Nil : PersistentList a
  Cons : (value : a) -> (left : PersistentList a) -> (right : PersistentList a) -> PersistentList a

public export
isEmpty : PersistentList a -> Bool
isEmpty Nil = True
isEmpty _ = False

public export
singleton : a -> PersistentList a
singleton value = Cons value Nil Nil

public export
cons : a -> PersistentList a -> PersistentList a
cons value list = Cons value Nil list

public export
head : PersistentList a -> Maybe a
head Nil = Nothing
head (Cons value _ _) = Just value

public export
tail : PersistentList a -> PersistentList a
tail Nil = Nil
tail (Cons _ Nil Nil) = Nil
tail (Cons _ left right) = catenate left right

public export
catenate : PersistentList a -> PersistentList a -> PersistentList a
catenate Nil ys = ys
catenate xs Nil = xs
catenate xs@(Cons _ _ Nil) ys = catenateHelper xs ys
catenate xs ys@(Cons _ Nil _) = catenateHelper ys xs
catenate xs@(Cons _ xl xr) ys@(Cons _ yl yr) =
  if xr == yl
    then Cons (head xr) xl yr
    else catenateHelper xs ys

private
catenateHelper : PersistentList a -> PersistentList a -> PersistentList a
catenateHelper Nil ys = ys
catenateHelper xs Nil = xs
catenateHelper xs@(Cons value xl xr) ys = Cons value xl (catenateHelper xr ys)