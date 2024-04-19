import Data.Word (Word8)
import Data.Bits ((.|.), (.&.))
import Data.ByteString (ByteString, packCStringLen)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

type BloomFilter = (Int, ByteString, [ByteString -> Int])

createBloomFilter :: Int -> [ByteString -> Int] -> BloomFilter
createBloomFilter size hashFuncs =
    (size, B.replicate (size `div` 8 + 1) 0, hashFuncs)

addToBloomFilter :: BloomFilter -> ByteString -> BloomFilter
addToBloomFilter (size, vector, hashFuncs) element =
    (size, B.foldl' updateBit vector (map (\f -> f element `mod` size) hashFuncs), hashFuncs)
    where
        updateBit vec bitIndex =
            let (beforeBytes, afterBytes) = B.splitAt (bitIndex `div` 8) vec
                byte = vec `BC.index` (bitIndex `div` 8)
                mask = 1 `shiftL` (bitIndex `mod` 8)
            in beforeBytes `B.snoc` (byte .|. mask) `B.append` afterBytes

elemInBloomFilter :: BloomFilter -> ByteString -> Bool
elemInBloomFilter (size, vector, hashFuncs) element =
    all (\f -> (vector `BC.index` (f element `mod` size `div` 8)) .&. (1 `shiftL` (f element `mod` size `mod` 8)) /= 0) hashFuncs