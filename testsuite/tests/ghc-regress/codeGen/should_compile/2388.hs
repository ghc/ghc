module Q where

import Data.Bits
import Data.Word
import Data.Int

test1 :: Word32 -> Char
test1 w | w .&. 0x80000000 /= 0 = 'a'
test1 _ = 'b'

-- this should use a testq instruction on x86_64
test2 :: Int64 -> Char
test2 w | w .&. (-3) /= 0 = 'a'
test2 _ = 'b'
