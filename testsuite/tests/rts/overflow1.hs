module Main where

import Data.Array.IO
import Data.Word

-- Try to overflow BLOCK_ROUND_UP in the computation of req_blocks in allocate()
-- Here we invoke allocate() via newByteArray# and the array package.
-- Request a number of bytes close to HS_WORD_MAX,
-- subtracting a few words for overhead in newByteArray#.
-- Allocate Word32s (rather than Word8s) to get around bounds-checking in array.
main = newArray (0,maxBound `div` 4 - 10) 0 :: IO (IOUArray Word Word32)
