-- |
-- Module      : Basement.String.Encoding.UTF16
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
module Basement.String.Encoding.UTF16
    ( UTF16(..)
    , UTF16_Invalid(..)
    ) where

import GHC.Prim
import GHC.Word
import GHC.Types
import Data.Bits
import qualified Prelude
import Basement.Compat.Base
import Basement.Types.OffsetSize
import Basement.Monad
import Basement.Numerical.Additive
import Basement.UArray
import Basement.UArray.Mutable (MUArray)
import Basement.MutableBuilder

import Basement.String.Encoding.Encoding

data UTF16_Invalid
    = InvalidContinuation
    | InvalidUnicode Char
  deriving (Show, Eq, Typeable)
instance Exception UTF16_Invalid

data UTF16 = UTF16

instance Encoding UTF16 where
    type Unit UTF16 = Word16
    type Error UTF16 = UTF16_Invalid
    encodingNext  _ = next
    encodingWrite _ = write


--
-- U+0000 to U+D7FF and U+E000 to U+FFFF : 1 bytes
-- U+10000 to U+10FFFF :
--    * 0x010000 is subtracted from the code point, leaving a 20-bit number in the range 0..0x0FFFFF.
--    * The top ten bits (a number in the range 0..0x03FF) are added to 0xD800 to give the first 16-bit code unit
--      or high surrogate, which will be in the range 0xD800..0xDBFF.
--    * The low ten bits (also in the range 0..0x03FF) are added to 0xDC00 to give the second 16-bit code unit
--      or low surrogate, which will be in the range 0xDC00..0xDFFF.

next :: (Offset Word16 -> Word16)
     -> Offset Word16
     -> Either UTF16_Invalid (Char, Offset Word16)
next getter off
    | h <  0xd800 = Right (toChar hh, off + Offset 1)
    | h >= 0xe000 = Right (toChar hh, off + Offset 1)
    | otherwise   = nextContinuation
  where
    h :: Word16
    !h@(W16# hh) = getter off
    toChar :: Word# -> Char
    toChar w = C# (chr# (word2Int# w))
    to32 :: Word16 -> Word32
    to32 (W16# w) = W32# w

    nextContinuation
        | cont >= 0xdc00 && cont < 0xe00 =
            let !(W32# w) = ((to32 h .&. 0x3ff) `shiftL` 10)
                         .|. (to32 cont .&. 0x3ff)
             in Right (toChar w, off + Offset 2)
        | otherwise = Left InvalidContinuation
      where
        cont :: Word16
        !cont = getter $ off + Offset 1

write :: (PrimMonad st, Monad st)
      => Char
      -> Builder (UArray Word16) (MUArray Word16) Word16 st err ()
write c
    | c < toEnum 0xd800   = builderAppend $ w16 c
    | c > toEnum 0x10000  = let (w1, w2) = wHigh c in builderAppend w1 >> builderAppend w2
    | c > toEnum 0x10ffff = throw $ InvalidUnicode c
    | c >= toEnum 0xe000  = builderAppend $ w16 c
    | otherwise = throw $ InvalidUnicode c
  where
    w16 :: Char -> Word16
    w16 (C# ch) = W16# (int2Word# (ord# ch))

    to16 :: Word32 -> Word16
    to16 = Prelude.fromIntegral

    wHigh :: Char -> (Word16, Word16)
    wHigh (C# ch) =
        let v = W32# (minusWord# (int2Word# (ord# ch)) 0x10000##)
         in (0xdc00 .|. to16 (v `shiftR` 10), 0xd800 .|. to16 (v .&. 0x3ff))
