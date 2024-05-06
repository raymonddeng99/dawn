module LSHHash

hash : String -> Int
hash str = sum $ map cast $ unpack str

hammingDistance : String -> String -> Int
hammingDistance str1 str2 = length $ filter (/=) $ zipWith (==) (unpack str1) (unpack str2)

lshFunc : Int -> Int -> String -> List Int
lshFunc k l str = [hash $ take l $ drop (i * length str `div` k) $ unpack str | i <- [0..k-1]]

buildLSHTable : Int -> Int -> List String -> Map Int (List String)
buildLSHTable k l strings = foldl (\table str => foldl (\table' hashVal => insert hashVal [str] table') table (lshFunc k l str)) empty strings

queryLSHTable : Int -> Int -> Map Int (List String) -> String -> List String
queryLSHTable k l table queryStr = nub $ concat $ map (\hashVal => maybe [] id $ lookup hashVal table >>= filter (\str => hammingDistance str queryStr <= l)) $ lshFunc k l queryStr