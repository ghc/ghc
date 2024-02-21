{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Unicode.Bits
-- Copyright   : (c) 2020 Andrew Lelechenko
--               (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
--
-- Maintainer  :  streamly@composewell.com
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Fast, static bitmap lookup utilities
--
-----------------------------------------------------------------------------

module GHC.Internal.Unicode.Bits
    ( lookupBit64,
      lookupIntN
    ) where

import GHC.Internal.Base (Bool, Int(..), Word(..), Eq(..))
import GHC.Internal.Bits (finiteBitSize, popCount)
import {-# SOURCE #-} GHC.Internal.ByteOrder
import GHC.Prim
       (Addr#,
        indexWordOffAddr#, indexWord8OffAddr#,
        andI#, uncheckedIShiftRL#,
        and#, word2Int#, uncheckedShiftL#,
        word8ToWord#, byteSwap#)
import GHC.Internal.Num ((-))

-- | @lookup64 addr index@ looks up the bit stored at bit index @index@ using a
-- bitmap starting at the address @addr@. Looks up the 64-bit word containing
-- the bit and then the bit in that word. The caller must make sure that the
-- 64-bit word at the byte address (addr + index / 64) * 8 is legally
-- accessible memory.
--
lookupBit64 :: Addr# -> Int -> Bool
lookupBit64 addr# (I# index#) = W# (word## `and#` bitMask##) /= 0
  where
    !fbs@(I# fbs#) = finiteBitSize (0 :: Word) - 1
    !(I# logFbs#) = case fbs of
      31 -> 5
      63 -> 6
      _  -> popCount fbs -- this is a really weird architecture

    wordIndex# = index# `uncheckedIShiftRL#` logFbs#
    word## = case targetByteOrder of
      BigEndian    -> byteSwap# (indexWordOffAddr# addr# wordIndex#)
      LittleEndian -> indexWordOffAddr# addr# wordIndex#
    bitIndex# = index# `andI#` fbs#
    bitMask## = 1## `uncheckedShiftL#` bitIndex#

{-| @lookupIntN addr index@ looks up for the @index@-th @8@-bits word in
the bitmap starting at @addr@, then convert it to an Int.

The caller must make sure that:

* @ceiling (addr + (n * 8))@ is legally accessible @Word8@.

@since base-0.3.0
-}
lookupIntN
  :: Addr# -- ^ Bitmap address
  -> Int   -- ^ Word index
  -> Int   -- ^ Resulting word as 'Int'
lookupIntN addr# (I# index#) =
  let word## = word8ToWord# (indexWord8OffAddr# addr# index#)
  in I# (word2Int# word##)
