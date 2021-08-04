-- |
-- Module      : Basement.String.Encoding.ASCII7
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE MagicHash #-}

module Basement.String.Encoding.ASCII7
    ( ASCII7(..)
    , ASCII7_Invalid(..)
    ) where

import Basement.Compat.Base
import Basement.Types.OffsetSize
import Basement.Numerical.Additive
import Basement.Monad

import GHC.Prim
import GHC.Word
import GHC.Types
import Basement.UArray
import Basement.UArray.Mutable (MUArray)
import Basement.MutableBuilder

import Basement.String.Encoding.Encoding

-- | validate a given byte is within ASCII characters encoring size
--
-- This function check the 8th bit is set to 0
--
isAscii :: Word8 -> Bool
isAscii (W8# w) = W8# (and# w 0x80## ) == 0
{-# INLINE isAscii #-}

data ASCII7_Invalid
    = ByteOutOfBound Word8
    | CharNotAscii   Char
  deriving (Typeable, Show, Eq)
instance Exception ASCII7_Invalid

data ASCII7 = ASCII7

instance Encoding ASCII7 where
    type Unit ASCII7 = Word8
    type Error ASCII7 = ASCII7_Invalid
    encodingNext  _ = next
    encodingWrite _ = write

-- | consume an Ascii7 char and return the Unicode point and the position
-- of the next possible Ascii7 char
--
next :: (Offset Word8 -> Word8)
          -- ^ method to access a given byte
     -> Offset Word8
          -- ^ index of the byte
     -> Either ASCII7_Invalid (Char, Offset Word8)
          -- ^ either successfully validated the ASCII char and returned the
          -- next index or fail with an error
next getter off
    | isAscii w8 = Right (toChar w, off + 1)
    | otherwise  = Left $ ByteOutOfBound w8
  where
    !w8@(W8# w) = getter off
    toChar :: Word# -> Char
    toChar a = C# (chr# (word2Int# a))

-- Write ascii char
--
-- > build 64 $ sequence_ write "this is a simple list of char..."
--
write :: (PrimMonad st, Monad st)
      => Char
           -- ^ expecting it to be a valid Ascii character.
           -- otherwise this function will throw an exception
      -> Builder (UArray Word8) (MUArray Word8) Word8 st err ()
write c
    | c < toEnum 0x80 = builderAppend $ w8 c
    | otherwise       = throw $ CharNotAscii c
  where
    w8 :: Char -> Word8
    w8 (C# ch) = W8# (int2Word# (ord# ch))
