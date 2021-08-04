-- |
-- Module      : Basement.Block.Mutable
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- A block of memory that contains elements of a type,
-- very similar to an unboxed array but with the key difference:
--
-- * It doesn't have slicing capability (no cheap take or drop)
-- * It consume less memory: 1 Offset, 1 CountOf, 1 Pinning status trimmed
-- * It's unpackable in any constructor
-- * It uses unpinned memory by default
--
-- It should be rarely needed in high level API, but
-- in lowlevel API or some data structure containing lots
-- of unboxed array that will benefit from optimisation.
--
-- Because it's unpinned, the blocks are compactable / movable,
-- at the expense of making them less friendly to interop with the C layer
-- as address.
--
-- Note that sadly the bytearray primitive type automatically create
-- a pinned bytearray if the size is bigger than a certain threshold
--
-- GHC Documentation associated:
--
-- includes/rts/storage/Block.h
--   * LARGE_OBJECT_THRESHOLD ((uint32_t)(BLOCK_SIZE * 8 / 10))
--   * BLOCK_SIZE   (1<<BLOCK_SHIFT)
--
-- includes/rts/Constant.h
--   * BLOCK_SHIFT  12
--
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
module Basement.Block.Mutable
    ( Block(..)
    , MutableBlock(..)
    , mutableLengthSize
    , mutableLength
    , mutableLengthBytes
    , mutableWithPtr
    , withMutablePtr
    , withMutablePtrHint
    , new
    , newPinned
    , mutableEmpty
    , iterSet
    , read
    , write
    , unsafeNew
    , unsafeWrite
    , unsafeRead
    , unsafeFreeze
    , unsafeThaw
    , unsafeCopyElements
    , unsafeCopyElementsRO
    , unsafeCopyBytes
    , unsafeCopyBytesRO
    , unsafeCopyBytesPtr
    -- * Foreign
    , copyFromPtr
    , copyToPtr
    ) where

import           GHC.Prim
import           GHC.Types
import           Basement.Compat.Base
import           Data.Proxy
import           Basement.Exception
import           Basement.Types.OffsetSize
import           Basement.Monad
import           Basement.Numerical.Additive
import           Basement.PrimType
import           Basement.Block.Base

-- | Set all mutable block element to a value
iterSet :: (PrimType ty, PrimMonad prim)
        => (Offset ty -> ty)
        -> MutableBlock ty (PrimState prim)
        -> prim ()
iterSet f ma = loop 0
  where
    !sz = mutableLength ma
    loop i
        | i .==# sz = pure ()
        | otherwise = unsafeWrite ma i (f i) >> loop (i+1)
    {-# INLINE loop #-}

mutableLengthSize :: PrimType ty => MutableBlock ty st -> CountOf ty
mutableLengthSize = mutableLength
{-# DEPRECATED mutableLengthSize "use mutableLength" #-}

-- | read a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
read :: (PrimMonad prim, PrimType ty) => MutableBlock ty (PrimState prim) -> Offset ty -> prim ty
read array n
    | isOutOfBound n len = primOutOfBound OOB_Read n len
    | otherwise          = unsafeRead array n
  where len = mutableLength array
{-# INLINE read #-}

-- | Write to a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
write :: (PrimMonad prim, PrimType ty) => MutableBlock ty (PrimState prim) -> Offset ty -> ty -> prim ()
write array n val
    | isOutOfBound n len = primOutOfBound OOB_Write n len
    | otherwise          = unsafeWrite array n val
  where
    len = mutableLengthSize array
{-# INLINE write #-}

-- | Copy from a pointer, @count@ elements, into the Mutable Block at a starting offset @ofs@
--
-- if the source pointer is invalid (size or bad allocation), bad things will happen
--
copyFromPtr :: forall prim ty . (PrimMonad prim, PrimType ty)
            => Ptr ty                           -- ^ Source Ptr of 'ty' to start of memory
            -> MutableBlock ty (PrimState prim) -- ^ Destination mutable block
            -> Offset ty                        -- ^ Start offset in the destination mutable block
            -> CountOf ty                       -- ^ Number of 'ty' elements
            -> prim ()
copyFromPtr src@(Ptr src#) mb@(MutableBlock mba) ofs count
    | end > sizeAsOffset arrSz = primOutOfBound OOB_MemCopy end arrSz
    | otherwise                = primitive $ \st -> (# copyAddrToByteArray# src# mba od# bytes# st, () #)
  where
    end = od `offsetPlusE` arrSz

    sz = primSizeInBytes (Proxy :: Proxy ty)
    !arrSz@(CountOf (I# bytes#)) = sizeOfE sz count
    !od@(Offset (I# od#)) = offsetOfE sz ofs

-- | Copy all the block content to the memory starting at the destination address
--
-- If the destination pointer is invalid (size or bad allocation), bad things will happen
copyToPtr :: forall ty prim . (PrimType ty, PrimMonad prim)
          => MutableBlock ty (PrimState prim) -- ^ The source mutable block to copy
          -> Offset ty                        -- ^ The source offset in the mutable block
          -> Ptr ty                           -- ^ The destination address where the copy is going to start
          -> CountOf ty                       -- ^ The number of bytes
          -> prim ()
copyToPtr mb@(MutableBlock mba) ofs dst@(Ptr dst#) count
    | srcEnd > sizeAsOffset arrSz = primOutOfBound OOB_MemCopy srcEnd arrSz
    | otherwise                = do
        blk <- unsafeFreeze mb
        let !(Block ba) = blk
        primitive $ \s1 -> (# copyByteArrayToAddr# ba os# dst# szBytes# s1, () #)
  where
    srcEnd = os `offsetPlusE` arrSz
    !os@(Offset (I# os#)) = offsetInBytes ofs
    !arrSz@(CountOf (I# szBytes#)) = mutableLengthBytes mb
