module Bug where

import Foreign hiding ( unsafePerformIO )
import Foreign.ForeignPtr
import Data.Char
import System.IO.Unsafe

data PackedString = PS !(ForeignPtr Word8) !Int !Int

(!) :: PackedString -> Int -> Word8
(PS x s _l) ! i
    = unsafePerformIO $ withForeignPtr x $ \p -> peekElemOff p (s+i)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

indexPS :: PackedString -> Int -> Char
indexPS theps i | i < 0 = error "Negative index in indexPS"
                | otherwise = w2c $ theps ! i

