module XFastTrie
  (XFastTrie,
    create,
    insert,
    predecessor
  ) where

import Data.Bits
import Data.Array.IO

data XFastTrie = XFastTrie
  { bits :: IOArray Int Bool
  , ones :: [Int]
  , size :: Int
  }

create :: Int -> IO XFastTrie
create w = do
  let u = 2 ^ w
  bits <- newArray (0, 2 * u - 2) False
  return $ XFastTrie bits [] 0

insert :: XFastTrie -> Int -> IO XFastTrie
insert (XFastTrie bits ones size) x =
  insertAux bits ones size x 0
 where
  insertAux bits ones size x i
    | i >= sizeOfIOArray bits = return $ XFastTrie bits ones (size + 1)
    | otherwise = do
      let bit = x .&. (1 `shiftL` i) /= 0
      let index = 1 `shiftL` (sizeOfIOArray bits - 1 - i)
      writeArray bits (index - 1) (bit || bits `indexArray` (index - 1))
      if bit
        then insertAux bits (index : ones) size x (i + 1)
        else insertAux bits ones size x (i + 1)

predecessor :: XFastTrie -> Int -> IO (Maybe Int)
predecessor (XFastTrie bits ones _) x = predAux bits ones x 0
 where
  predAux bits ones x i
    | i >= sizeOfIOArray bits = return Nothing
    | otherwise = do
      let index = 1 `shiftL` (sizeOfIOArray bits - 1 - i)
      if bits `indexArray` (index - 1)
        then do
          let leftChild = 2 * index
          let rightChild = leftChild + 1
          if bits `indexArray` (rightChild - 1)
            then predAux bits ones x (i + 1)
            else return $ Just $ head ones
        else predAux bits ones x (i + 1)