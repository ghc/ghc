-- |
-- Module      : Data.Memory.Internal.Compat
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
-- This module try to keep all the difference between versions of base
-- or other needed packages, so that modules don't need to use CPP
--
{-# LANGUAGE CPP #-}
module Data.Memory.Internal.Compat
    ( unsafeDoIO
    , popCount
    , unsafeShiftL
    , unsafeShiftR
    , byteSwap64
    , byteSwap32
    , byteSwap16
    ) where

import System.IO.Unsafe
import Data.Word
import Data.Bits

-- | perform io for hashes that do allocation and ffi.
-- unsafeDupablePerformIO is used when possible as the
-- computation is pure and the output is directly linked
-- to the input. we also do not modify anything after it has
-- been returned to the user.
unsafeDoIO :: IO a -> a
#if __GLASGOW_HASKELL__ > 704
unsafeDoIO = unsafeDupablePerformIO
#else
unsafeDoIO = unsafePerformIO
#endif

#if !(MIN_VERSION_base(4,5,0))
popCount :: Word64 -> Int
popCount n = loop 0 n
  where loop c 0 = c
        loop c i = loop (c + if testBit c 0 then 1 else 0) (i `shiftR` 1)
#endif

#if !(MIN_VERSION_base(4,7,0))
byteSwap64 :: Word64 -> Word64
byteSwap64 w =
        (w `shiftR` 56)                  .|. (w `shiftL` 56)
    .|. ((w `shiftR` 40) .&. 0xff00)     .|. ((w .&. 0xff00) `shiftL` 40)
    .|. ((w `shiftR` 24) .&. 0xff0000)   .|. ((w .&. 0xff0000) `shiftL` 24)
    .|. ((w `shiftR` 8)  .&. 0xff000000) .|. ((w .&. 0xff000000) `shiftL` 8)
#endif

#if !(MIN_VERSION_base(4,7,0))
byteSwap32 :: Word32 -> Word32
byteSwap32 w =
        (w `shiftR` 24)
    .|. (w `shiftL` 24)
    .|. ((w `shiftR` 8) .&. 0xff00)
    .|. ((w .&. 0xff00) `shiftL` 8)
#endif

#if !(MIN_VERSION_base(4,7,0))
byteSwap16 :: Word16 -> Word16
byteSwap16 w =
    (w `shiftR` 8) .|. (w `shiftL` 8)
#endif

#if !(MIN_VERSION_base(4,5,0))
unsafeShiftL :: Bits a => a -> Int -> a
unsafeShiftL = shiftL

unsafeShiftR :: Bits a => a -> Int -> a
unsafeShiftR = shiftR
#endif

