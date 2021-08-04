-- |
-- Module      : Crypto.Internal.CompatPrim
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Compat
--
-- This module tries to keep all the difference between versions of ghc primitive
-- or other needed packages, so that modules don't need to use CPP.
--
-- Note that MagicHash and CPP conflicts in places, making it "more interesting"
-- to write compat code for primitives.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Crypto.Internal.CompatPrim
    ( be32Prim
    , le32Prim
    , byteswap32Prim
    , booleanPrim
    , convert4To32
    ) where

import GHC.Prim
#if !defined(ARCH_IS_LITTLE_ENDIAN) && !defined(ARCH_IS_BIG_ENDIAN)
import Data.Memory.Endian (getSystemEndianness, Endianness(..))
#endif

-- | Byteswap Word# to or from Big Endian
--
-- On a big endian machine, this function is a nop.
be32Prim :: Word# -> Word#
#ifdef ARCH_IS_LITTLE_ENDIAN
be32Prim = byteswap32Prim
#elif defined(ARCH_IS_BIG_ENDIAN)
be32Prim = id
#else
be32Prim w = if getSystemEndianness == LittleEndian then byteswap32Prim w else w
#endif

-- | Byteswap Word# to or from Little Endian
--
-- On a little endian machine, this function is a nop.
le32Prim :: Word# -> Word#
#ifdef ARCH_IS_LITTLE_ENDIAN
le32Prim w = w
#elif defined(ARCH_IS_BIG_ENDIAN)
le32Prim = byteswap32Prim
#else
le32Prim w = if getSystemEndianness == LittleEndian then w else byteswap32Prim w
#endif

-- | Simple compatibility for byteswap the lower 32 bits of a Word#
-- at the primitive level
byteswap32Prim :: Word# -> Word#
#if __GLASGOW_HASKELL__ >= 708
byteswap32Prim w = byteSwap32# w
#else
byteswap32Prim w =
    let !a =       uncheckedShiftL# w 24#
        !b = and# (uncheckedShiftL# w 8#) 0x00ff0000##
        !c = and# (uncheckedShiftRL# w 8#) 0x0000ff00##
        !d = and# (uncheckedShiftRL# w 24#) 0x000000ff##
     in or# a (or# b (or# c d))
#endif

-- | Combine 4 word8 [a,b,c,d] to a word32 representing [a,b,c,d]
convert4To32 :: Word# -> Word# -> Word# -> Word#
             -> Word#
convert4To32 a b c d = or# (or# c1 c2) (or# c3 c4)
  where
#ifdef ARCH_IS_LITTLE_ENDIAN
        !c1 = uncheckedShiftL# a 24#
        !c2 = uncheckedShiftL# b 16#
        !c3 = uncheckedShiftL# c 8#
        !c4 = d
#elif defined(ARCH_IS_BIG_ENDIAN)
        !c1 = uncheckedShiftL# d 24#
        !c2 = uncheckedShiftL# c 16#
        !c3 = uncheckedShiftL# b 8#
        !c4 = a
#else
        !c1
            | getSystemEndianness == LittleEndian = uncheckedShiftL# a 24#
            | otherwise                           = uncheckedShiftL# d 24#
        !c2
            | getSystemEndianness == LittleEndian = uncheckedShiftL# b 16#
            | otherwise                           = uncheckedShiftL# c 16#
        !c3
            | getSystemEndianness == LittleEndian = uncheckedShiftL# c 8#
            | otherwise                           = uncheckedShiftL# b 8#
        !c4
            | getSystemEndianness == LittleEndian = d
            | otherwise                           = a
#endif

-- | Simple wrapper to handle pre 7.8 and future, where
-- most comparaison functions don't returns a boolean
-- anymore.
#if __GLASGOW_HASKELL__ >= 708
booleanPrim :: Int# -> Bool
booleanPrim v = tagToEnum# v
#else
booleanPrim :: Bool -> Bool
booleanPrim b = b
#endif
