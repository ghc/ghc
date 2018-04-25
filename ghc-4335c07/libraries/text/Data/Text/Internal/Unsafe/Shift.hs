{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Unsafe.Shift
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Fast, unchecked bit shifting functions.

module Data.Text.Internal.Unsafe.Shift
    (
      UnsafeShift(..)
    ) where

-- import qualified Data.Bits as Bits
import GHC.Base
import GHC.Word

-- | This is a workaround for poor optimisation in GHC 6.8.2.  It
-- fails to notice constant-width shifts, and adds a test and branch
-- to every shift.  This imposes about a 10% performance hit.
--
-- These functions are undefined when the amount being shifted by is
-- greater than the size in bits of a machine Int#.
class UnsafeShift a where
    shiftL :: a -> Int -> a
    shiftR :: a -> Int -> a

instance UnsafeShift Word16 where
    {-# INLINE shiftL #-}
    shiftL (W16# x#) (I# i#) = W16# (narrow16Word# (x# `uncheckedShiftL#` i#))

    {-# INLINE shiftR #-}
    shiftR (W16# x#) (I# i#) = W16# (x# `uncheckedShiftRL#` i#)

instance UnsafeShift Word32 where
    {-# INLINE shiftL #-}
    shiftL (W32# x#) (I# i#) = W32# (narrow32Word# (x# `uncheckedShiftL#` i#))

    {-# INLINE shiftR #-}
    shiftR (W32# x#) (I# i#) = W32# (x# `uncheckedShiftRL#` i#)

instance UnsafeShift Word64 where
    {-# INLINE shiftL #-}
    shiftL (W64# x#) (I# i#) = W64# (x# `uncheckedShiftL64#` i#)

    {-# INLINE shiftR #-}
    shiftR (W64# x#) (I# i#) = W64# (x# `uncheckedShiftRL64#` i#)

instance UnsafeShift Int where
    {-# INLINE shiftL #-}
    shiftL (I# x#) (I# i#) = I# (x# `iShiftL#` i#)

    {-# INLINE shiftR #-}
    shiftR (I# x#) (I# i#) = I# (x# `iShiftRA#` i#)

{-
instance UnsafeShift Integer where
    {-# INLINE shiftL #-}
    shiftL = Bits.shiftL

    {-# INLINE shiftR #-}
    shiftR = Bits.shiftR
-}
