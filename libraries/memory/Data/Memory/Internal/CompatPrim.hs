-- |
-- Module      : Data.Memory.Internal.CompatPrim
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Compat
--
-- This module try to keep all the difference between versions of ghc primitive
-- or other needed packages, so that modules don't need to use CPP.
--
-- Note that MagicHash and CPP conflicts in places, making it "more interesting"
-- to write compat code for primitives
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.Memory.Internal.CompatPrim
    ( be32Prim
    , le32Prim
    , byteswap32Prim
    , booleanPrim
    ) where

import GHC.Prim

-- | byteswap Word# to or from Big Endian
--
-- on a big endian machine, this function is a nop.
be32Prim :: Word# -> Word#
#ifdef ARCH_IS_LITTLE_ENDIAN
be32Prim = byteswap32Prim
#else
be32Prim w = w
#endif

-- | byteswap Word# to or from Little Endian
--
-- on a little endian machine, this function is a nop.
le32Prim :: Word# -> Word#
#ifdef ARCH_IS_LITTLE_ENDIAN
le32Prim w = w
#else
le32Prim = byteswap32Prim
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
{-# INLINE booleanPrim #-}
