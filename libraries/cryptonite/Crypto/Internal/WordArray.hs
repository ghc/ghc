-- |
-- Module      : Crypto.Internal.WordArray
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
-- Small and self contained array representation
-- with limited safety for internal use.
--
-- The array produced should never be exposed to the user directly.
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Crypto.Internal.WordArray
    ( Array8
    , Array32
    , Array64
    , MutableArray32
    , array8
    , array32
    , array32FromAddrBE
    , allocArray32AndFreeze
    , mutableArray32
    , array64
    , arrayRead8
    , arrayRead32
    , arrayRead64
    , mutableArrayRead32
    , mutableArrayWrite32
    , mutableArrayWriteXor32
    , mutableArray32FromAddrBE
    , mutableArray32Freeze
    ) where

import Data.Word
import Data.Bits (xor)
import Crypto.Internal.Compat
import Crypto.Internal.CompatPrim
import GHC.Prim
import GHC.Types
import GHC.Word

-- | Array of Word8
data Array8 = Array8 Addr#

-- | Array of Word32
data Array32 = Array32 ByteArray#

-- | Array of Word64
data Array64 = Array64 ByteArray#

-- | Array of mutable Word32
data MutableArray32 = MutableArray32 (MutableByteArray# RealWorld)

-- | Create an array of Word8 aliasing an Addr#
array8 :: Addr# -> Array8
array8 = Array8

-- | Create an Array of Word32 of specific size from a list of Word32
array32 :: Int -> [Word32] -> Array32
array32 n l = unsafeDoIO (mutableArray32 n l >>= mutableArray32Freeze)
{-# NOINLINE array32 #-}

-- | Create an Array of BE Word32 aliasing an Addr
array32FromAddrBE :: Int -> Addr# -> Array32
array32FromAddrBE n a =
    unsafeDoIO (mutableArray32FromAddrBE n a >>= mutableArray32Freeze)
{-# NOINLINE array32FromAddrBE #-}

-- | Create an Array of Word32 using an initializer
allocArray32AndFreeze :: Int -> (MutableArray32 -> IO ()) -> Array32
allocArray32AndFreeze n f =
    unsafeDoIO (mutableArray32 n [] >>= \m -> f m >> mutableArray32Freeze m)
{-# NOINLINE allocArray32AndFreeze #-}

-- | Create an Array of Word64 of specific size from a list of Word64
array64 :: Int -> [Word64] -> Array64
array64 (I# n) l = unsafeDoIO $ IO $ \s ->
    case newAlignedPinnedByteArray# (n *# 8#) 8# s of
        (# s', mbarr #) -> loop 0# s' mbarr l
  where
        loop _ st mb [] = freezeArray mb st
        loop i st mb ((W64# x):xs)
            | booleanPrim (i ==# n) = freezeArray mb st
            | otherwise =
                let !st' = writeWord64Array# mb i x st
                 in loop (i +# 1#) st' mb xs
        freezeArray mb st =
            case unsafeFreezeByteArray# mb st of
                (# st', b #) -> (# st', Array64 b #)
{-# NOINLINE array64 #-}

-- | Create a Mutable Array of Word32 of specific size from a list of Word32
mutableArray32 :: Int -> [Word32] -> IO MutableArray32
mutableArray32 (I# n) l = IO $ \s ->
    case newAlignedPinnedByteArray# (n *# 4#) 4# s of
        (# s', mbarr #) -> loop 0# s' mbarr l
  where
        loop _ st mb [] = (# st, MutableArray32 mb #)
        loop i st mb ((W32# x):xs)
            | booleanPrim (i ==# n) = (# st, MutableArray32 mb #)
            | otherwise =
                let !st' = writeWord32Array# mb i x st
                 in loop (i +# 1#) st' mb xs

-- | Create a Mutable Array of BE Word32 aliasing an Addr
mutableArray32FromAddrBE :: Int -> Addr# -> IO MutableArray32
mutableArray32FromAddrBE (I# n) a = IO $ \s ->
    case newAlignedPinnedByteArray# (n *# 4#) 4# s of
        (# s', mbarr #) -> loop 0# s' mbarr
  where
        loop i st mb
            | booleanPrim (i ==# n) = (# st, MutableArray32 mb #)
            | otherwise             =
                let !st' = writeWord32Array# mb i (be32Prim (indexWord32OffAddr# a i)) st
                 in loop (i +# 1#) st' mb

-- | freeze a Mutable Array of Word32 into a immutable Array of Word32
mutableArray32Freeze :: MutableArray32 -> IO Array32
mutableArray32Freeze (MutableArray32 mb) = IO $ \st ->
    case unsafeFreezeByteArray# mb st of
        (# st', b #) -> (# st', Array32 b #)

-- | Read a Word8 from an Array
arrayRead8 :: Array8 -> Int -> Word8
arrayRead8 (Array8 a) (I# o) = W8# (indexWord8OffAddr# a o)
{-# INLINE arrayRead8 #-}

-- | Read a Word32 from an Array
arrayRead32 :: Array32 -> Int -> Word32
arrayRead32 (Array32 b) (I# o) = W32# (indexWord32Array# b o)
{-# INLINE arrayRead32 #-}

-- | Read a Word64 from an Array
arrayRead64 :: Array64 -> Int -> Word64
arrayRead64 (Array64 b) (I# o) = W64# (indexWord64Array# b o)
{-# INLINE arrayRead64 #-}

-- | Read a Word32 from a Mutable Array of Word32
mutableArrayRead32 :: MutableArray32 -> Int -> IO Word32
mutableArrayRead32 (MutableArray32 m) (I# o) = IO $ \s -> case readWord32Array# m o s of (# s', e #) -> (# s', W32# e #)
{-# INLINE mutableArrayRead32 #-}

-- | Write a Word32 from a Mutable Array of Word32
mutableArrayWrite32 :: MutableArray32 -> Int -> Word32 -> IO ()
mutableArrayWrite32 (MutableArray32 m) (I# o) (W32# w) = IO $ \s -> let !s' = writeWord32Array# m o w s in (# s', () #)
{-# INLINE mutableArrayWrite32 #-}

-- | Write into the Mutable Array of Word32 by combining through xor the current value and the new value.
--
-- > x[i] = x[i] xor value
mutableArrayWriteXor32 :: MutableArray32 -> Int -> Word32 -> IO ()
mutableArrayWriteXor32 m o w =
    mutableArrayRead32 m o >>= \wOld -> mutableArrayWrite32 m o (wOld `xor` w)
{-# INLINE mutableArrayWriteXor32 #-}
