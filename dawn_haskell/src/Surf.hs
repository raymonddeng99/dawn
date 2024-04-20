module SurfBase (lookup, build) where

import qualified Data.Map.Strict as Map

build :: [String] -> Map.Map Char (Map.Map Char ())
build keys = foldl insert Map.empty keys
  where
    insert trie key = foldl' (\trie' c -> Map.alter (maybe (Just Map.empty) (const $ Just Map.empty)) c trie') trie key

lookup :: Map.Map Char (Map.Map Char ()) -> String -> Bool
lookup trie key = go trie key
  where
    go _ [] = True
    go trie (c:cs) = case Map.lookup c trie of
      Nothing -> False
      Just child -> go child cs