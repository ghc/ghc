module GHC.Utils.Word64 (
    intToWord64,
    word64ToInt,
    truncateWord64ToWord32,
  ) where

import GHC.Prelude
import GHC.Utils.Panic.Plain (assert)
import GHC.Utils.Misc (HasDebugCallStack)

import Data.Word

intToWord64 :: HasDebugCallStack => Int -> Word64
intToWord64 x = assert (0 <= x) (fromIntegral x)

word64ToInt :: HasDebugCallStack => Word64 -> Int
word64ToInt x = assert (x <= fromIntegral (maxBound :: Int)) (fromIntegral x)

truncateWord64ToWord32 :: Word64 -> Word32
truncateWord64ToWord32 = fromIntegral
