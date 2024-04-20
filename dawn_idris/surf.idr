import Data.SortedMap

SurfBase : Type
SurfBase = SortedMap Char SurfBase

build : List String -> SurfBase
build keys = foldl insertKey empty keys
  where
    insertKey : SurfBase -> String -> SurfBase
    insertKey trie str = foldl insertChar trie (unpack str)

    insertChar : SurfBase -> Char -> SurfBase
    insertChar trie char = case lookup char trie of
      Nothing => insert char empty trie
      Just child => insert char child trie

lookup : SurfBase -> String -> Bool
lookup trie str = go trie (unpack str)
  where
    go : SurfBase -> List Char -> Bool
    go trie [] = True
    go trie (x :: xs) = case lookup x trie of
      Nothing => False
      Just child => go child xs