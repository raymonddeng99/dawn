module XFastTrie

import Data.Vect

public export
record XFastTrie (w : Nat) where
  constructor MkXFastTrie
  bits : Vect (2 ^ (w + 1) - 1) Bool
  ones : List Nat
  size : Nat

export
create : (w : Nat) -> XFastTrie w
create w = MkXFastTrie (replicate (2 ^ (w + 1) - 1) False) [] 0

export
insert : XFastTrie w -> Nat -> XFastTrie w
insert trie@(MkXFastTrie bits ones size) x = insertAux trie x 0
  where
    insertAux : XFastTrie w -> Nat -> Nat -> XFastTrie w
    insertAux trie@(MkXFastTrie bits ones size) x i =
      if i >= length bits
         then MkXFastTrie bits ones (size + 1)
         else let bit = (x .& (1 `shiftL` i)) /= 0
                  index = 1 `shiftL` (length bits - 1 - i)
                  newBits = updateAt (index - 1) (bits ! (index - 1) || bit) bits
              in if bit
                    then insertAux (MkXFastTrie newBits (index :: ones) size) x (i + 1)
                    else insertAux (MkXFastTrie newBits ones size) x (i + 1)

export
predecessor : XFastTrie w -> Nat -> Maybe Nat
predecessor (MkXFastTrie bits ones size) x = predAux bits ones x 0
  where
    predAux : Vect (2 ^ (w + 1) - 1) Bool -> List Nat -> Nat -> Nat -> Maybe Nat
    predAux bits ones x i =
      if i >= length bits
         then Nothing
         else let index = 1 `shiftL` (length bits - 1 - i)
              in if index `isElem` bits
                    then let leftChild = 2 * index
                             rightChild = leftChild + 1
                         in if rightChild `isElem` bits
                               then predAux bits ones x (i + 1)
                               else Just (head ones)
                    else predAux bits ones x (i + 1)