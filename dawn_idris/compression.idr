module BWT

import Data.Strings
import Data.List
import Data.Vect

lastChar : String -> Char
lastChar s = last $ sort $ map rotate s
 where rotate : String -> String
       rotate xs = drop i xs ++ take i xs
         where i = length xs

findIndex : String -> List String -> Maybe Nat
findIndex s rotations = elemIndex s rotations

encode : String -> (String, Nat)
encode s = (encoded, idx)
 where
   n : Nat
   n = length s

   rotations : Vect n String
   rotations = map rotate $ range 0 (cast n)
     where rotate : Nat -> String
           rotate i = substr (cast i) (length s - i) s ++ substr 0 (cast i) s

   encoded : String
   encoded = pack $ map last $ sort $ toList rotations

   idx : Nat
   idx = fromMaybe 0 $ findIndex s $ toList rotations

decode : (String, Nat) -> String
decode (encoded, idx) = pack $ map last $ sort $ toList $ map rebuild $ range 0 (cast n)
 where
   n : Nat
   n = length encoded

   rebuild : Nat -> String
   rebuild i = singleton (index encoded i) ++ lastChar (substr 0 (cast i) encoded ++ substr (cast $ i + 1) (length encoded - i - 1) encoded)