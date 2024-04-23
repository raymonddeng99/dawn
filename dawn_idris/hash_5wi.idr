import Data.Vect

universal_hash : (Int, Int, Int, Int, Int) -> Int -> Int
universal_hash (a, b, c, d, e) key =
  let m = 2 ^ 32
      hash = a * key + b * key^2 + c * key^3 + d * key^4 + e * key^5
  in hash `mod` m

linear_probe : Vect n (Maybe a) -> Int -> (Fin n, Vect n (Maybe a))
linear_probe table key = loop 0 (universal_hash (randoms 5) key)
  where
    randoms : List Int -> List Int
    randoms [] = []
    randoms (x :: xs) = x :: randoms xs

    loop : Int -> Int -> (Fin n, Vect n (Maybe a))
    loop i hash =
      let index = fin (hash + i