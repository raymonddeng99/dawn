module LSHHash
  ( hash
  , hammingDistance
  , lshFunc
  , buildLSHTable
  , queryLSHTable
  ) where

import qualified Data.HashTable.IO as H
import Data.List (nub)
import Data.Char (ord)

hash :: String -> Int
hash = sum . map ord

hammingDistance :: String -> String -> Int
hammingDistance str1 str2 = length $ filter (uncurry (/=)) $ zip str1 str2

lshFunc :: Int -> Int -> String -> [Int]
lshFunc k l str = [hash $ take l $ drop (i * len `div` k) str | i <- [0..k-1]]
  where len = length str

buildLSHTable :: Int -> Int -> [String] -> IO (H.HashTable Int [String])
buildLSHTable k l strings = do
  table <- H.new
  mapM_ (\str -> mapM_ (\hashVal -> H.insert table hashVal [str]) $ lshFunc k l str) strings
  return table

queryLSHTable :: Int -> Int -> H.HashTable Int [String] -> String -> IO [String]
queryLSHTable k l table queryStr = do
  let hashes = lshFunc k l queryStr
  candidates <- concat <$> mapM (H.lookup table) hashes
  return $ filter (\str -> hammingDistance str queryStr <= l) $ nub candidates