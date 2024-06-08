{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE BlockArguments #-}

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
    ( lookupIntN
    , lookupBit64
    , newByteArrayFromWord8List
    , byteArrayLookupIntN
    , copyAddrToWord8List
    , UnicodeByteArray
    )
    where

import GHC.Internal.Bits (finiteBitSize, popCount)
import {-# SOURCE #-} GHC.Internal.ByteOrder
import GHC.Prim
import GHC.Internal.ST
import GHC.Internal.Base
import GHC.Internal.Num
import GHC.Internal.List
import GHC.Internal.Word

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

data UnicodeByteArray = UnicodeByteArray !ByteArray#

byteArrayLookupIntN :: UnicodeByteArray -> Int -> Int
byteArrayLookupIntN ba idx
  = let !(UnicodeByteArray addr) = ba
  in lookupIntN (byteArrayContents# addr) idx

newByteArrayFromWord8List :: [Word8] -> UnicodeByteArray
newByteArrayFromWord8List xs = runST $ ST \s0 ->
  case newPinnedByteArray# len s0 of
    !(# s1, mba #) ->
      let s2 = fillByteArray mba 0# xs s1
      in case unsafeFreezeByteArray# mba s2 of
        !(# s3, fba #) -> (# s3, UnicodeByteArray fba #)
  where
    !(I# len) = length xs

    fillByteArray _ _ [] s = s
    fillByteArray mba i (y:ys) s =
      let !(W8# y#) = y
          s' = writeWord8Array# mba i y# s
      in fillByteArray mba (i +# 1#) ys s'

copyAddrToWord8List :: Addr# -> Int -> [Word8]
copyAddrToWord8List addr !len@(I# len') = runST $ ST \s0 ->
  case newByteArray# len' s0 of
    !(# s1, mba #) ->
      let s2 = copyAddrToByteArray# addr mba 0# len' s1
      in case unsafeFreezeByteArray# mba s2 of
        !(# s3, fba #) -> (# s3, readByteFromArray fba 0 len #)
  where
    readByteFromArray :: ByteArray# -> Int -> Int -> [Word8]
    readByteFromArray ba !from@(I# from') to =
      W8# (indexWord8Array# ba from') :
        if from == (to - 1)
          then []
          else readByteFromArray ba (from + 1) to
