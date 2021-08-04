{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Word.Compat (
  module Base
, byteSwap16
, byteSwap32
, byteSwap64
) where

import Data.Word as Base

#if !(MIN_VERSION_base(4,7,0))
import Data.Bits

-- | Swap bytes in 'Word16'.
--
-- /Since: 4.7.0.0/
byteSwap16 :: Word16 -> Word16
byteSwap16 w = ((w `shiftR` 8) .&. 0x00ff)
           .|. ((w .&. 0x00ff) `shiftL` 8)

-- | Reverse order of bytes in 'Word32'.
--
-- /Since: 4.7.0.0/
byteSwap32 :: Word32 -> Word32
byteSwap32 w = ((w .&. 0xff000000) `shiftR` 24)
           .|. ((w .&. 0x00ff0000) `shiftR` 8)
           .|. ((w .&. 0x0000ff00) `shiftL` 8)
           .|. ((w .&. 0x000000ff) `shiftL` 24)

-- | Reverse order of bytes in 'Word64'.
--
-- /Since: 4.7.0.0/
byteSwap64 :: Word64 -> Word64
byteSwap64 w = ((w .&. 0xff00000000000000) `shiftR` 56)
           .|. ((w .&. 0x00ff000000000000) `shiftR` 40)
           .|. ((w .&. 0x0000ff0000000000) `shiftR` 24)
           .|. ((w .&. 0x000000ff00000000) `shiftR` 8)
           .|. ((w .&. 0x00000000ff000000) `shiftL` 8)
           .|. ((w .&. 0x0000000000ff0000) `shiftL` 24)
           .|. ((w .&. 0x000000000000ff00) `shiftL` 40)
           .|. ((w .&. 0x00000000000000ff) `shiftL` 56)
#endif
