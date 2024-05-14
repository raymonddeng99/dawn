module EulerTourTree

import Data.Vect

data EulerTourTree : Type -> Type where
  MkEulerTourTree : (tour : Vect n elem) ->
                    (first : Vect n Nat) ->
                    (last : Vect n Nat) ->
                    (level : Vect n Nat) ->
                    EulerTourTree elem

buildEulerTour : Tree elem -> Vect n elem
buildEulerTour (Node val forest) = val :: concat (map buildEulerTour forest) ++ [val]
buildEulerTour Empty = []

buildEulerTourTree : (tour : Vect n elem) -> EulerTourTree elem
buildEulerTourTree tour = MkEulerTourTree tour first last level
  where
    first : Vect n Nat
    first = buildFirst tour 0 [] (-1)

    last : Vect n Nat
    last = buildLast tour (len tour - 1) []

    level : Vect n Nat
    level = buildLevel tour 0 []

    buildFirst : Vect n elem -> Nat -> List Nat -> Nat -> Vect n Nat
    buildFirst [] _ acc _ = pack acc
    buildFirst (x :: xs) i acc j =
      case j of
           (-1) => buildFirst xs (i + 1) (i :: acc) i
           _    => buildFirst xs (i + 1) acc j

    buildLast : Vect n elem -> Nat -> List Nat -> Vect n Nat
    buildLast [] _ acc = pack acc
    buildLast (x :: xs) i acc =
      let j = findLast x xs i
      in buildLast xs j (j :: acc)

    buildLevel : Vect n elem -> Nat -> List Nat -> Vect n Nat
    buildLevel [] _ acc = pack acc
    buildLevel (x :: xs) i acc =
      let j = findLast x xs i
      in if j == -1
           then buildLevel xs (i + 1) (0 :: acc)
           else buildLevel xs j ((i - j) :: acc)

    findLast : elem -> Vect n elem -> Nat -> Nat
    findLast x [] i = -1
    findLast x (y :: ys) i =
      if x == y
         then i
         else findLast x ys (i - 1)