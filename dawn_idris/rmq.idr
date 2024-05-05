module RMQ

import Data.Vect

log2 : Nat -> Nat
log2 n = case isEven n of
           True => 1 + log2 (shift n)
           False => 0
  where
    isEven : Nat -> Bool
    isEven 0 = True
    isEven 1 = False
    isEven n = isEven (shift n)

    shift : Nat -> Nat
    shift 0 = 0
    shift n = prim__shr_Nat n 1

precomputeRMQ : Vect n Int -> Vect m Int -> Vect m Int -> Vect (log2 m + 1) (Vect m Int)
precomputeRMQ values euler first = go 0 (replicate m 0)
  where
    m : Nat
    m = length euler

    go : Nat -> Vect m Int -> Vect (S n) (Vect m Int)
    go n rmq
      = if n == log2 m + 1
           then []
           else let rmq' = case n == 0 of
                                True => [0..m-1]
                                False => zipWith (\x, y => min x y) (rmq !!! (n-1)) (drop (1 `shiftL` (n-1)) (rmq !!! (n-1)))
                  in rmq' :: go (S n) rmq'

query : Vect n Int -> Vect m Int -> Vect m Int -> Vect (log2 m + 1) (Vect m Int) -> Nat -> Nat -> Int
query values euler first rmq l r
  = let l' = min l r
        r' = max l r
        k = log2 (r' - l' + 1)
        x = index (rmq !! k) (first !! l')
        y = index (rmq !! k) (first !! r' - (1 `shiftL` k) + 1)
    in min (values !! index euler x) (values !! index euler y)

fromTree : Vect n Int -> (Vect m Int, Vect m Int, Vect (log2 m + 1) (Vect m Int))
fromTree values
  = let n = length values
        euler = concat [[i, i+1] | i <- [0, 2..2*n-2]]
        first = zipWith (\i, x => i) [0..2*n-1] (drop 1 euler)
        rmq = precomputeRMQ values euler first
    in (euler, first, rmq)