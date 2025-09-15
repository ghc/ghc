{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Streamly.Internal.Ring.Foreign
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Ring
    ( Ring(..)

    -- * Construction
    , new
    , advance
    , moveBy
    , startOf

    -- * Modification
    , unsafeInsert

    -- * Folds
    , unsafeFoldRing
    , unsafeFoldRingM
    , unsafeFoldRingFullM
    , unsafeFoldRingNM

    -- * Fast Byte Comparisons
    , unsafeEqArray
    , unsafeEqArrayN
    ) where

import Control.Exception (assert)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)
import GHC.Ptr (Ptr(..))
import Prelude hiding (length, concat)

import Control.Monad.IO.Class (MonadIO(..))

import qualified Array as A

-- | A ring buffer is a mutable array of fixed size. Initially the array is
-- empty, with ringStart pointing at the start of allocated memory. We call the
-- next location to be written in the ring as ringHead. Initially ringHead ==
-- ringStart. When the first item is added, ringHead points to ringStart +
-- sizeof item. When the buffer becomes full ringHead would wrap around to
-- ringStart. When the buffer is full, ringHead always points at the oldest
-- item in the ring and the newest item added always overwrites the oldest
-- item.
--
-- When using it we should keep in mind that a ringBuffer is a mutable data
-- structure. We should not leak out references to it for immutable use.
--
data Ring a = Ring
    { ringStart :: {-# UNPACK #-} !(ForeignPtr a) -- first address
    , ringBound :: {-# UNPACK #-} !(Ptr a)        -- first address beyond allocated memory
    }

-- | Get the first address of the ring as a pointer.
startOf :: Ring a -> Ptr a
startOf = unsafeForeignPtrToPtr . ringStart

-- | Create a new ringbuffer and return the ring buffer and the ringHead.
-- Returns the ring and the ringHead, the ringHead is same as ringStart.
{-# INLINE new #-}
new :: forall a. Storable a => Int -> IO (Ring a, Ptr a)
new count = do
    let size = count * sizeOf (undefined :: a)
    fptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: a))
    let p = unsafeForeignPtrToPtr fptr
    return (Ring
        { ringStart = fptr
        , ringBound = p `plusPtr` size
        }, p)

-- | Advance the ringHead by 1 item, wrap around if we hit the end of the
-- array.
{-# INLINE advance #-}
advance :: forall a. Storable a => Ring a -> Ptr a -> Ptr a
advance Ring{..} ringHead =
    let ptr = ringHead `plusPtr` sizeOf (undefined :: a)
    in if ptr <  ringBound
       then ptr
       else unsafeForeignPtrToPtr ringStart

-- | Move the ringHead by n items. The direction depends on the sign on whether
-- n is positive or negative. Wrap around if we hit the beginning or end of the
-- array.
{-# INLINE moveBy #-}
moveBy :: forall a. Storable a => Int -> Ring a -> Ptr a -> Ptr a
moveBy by Ring {..} ringHead = ringStartPtr `plusPtr` advanceFromHead

    where

    elemSize = sizeOf (undefined :: a)
    ringStartPtr = unsafeForeignPtrToPtr ringStart
    lenInBytes = ringBound `minusPtr` ringStartPtr
    offInBytes = ringHead `minusPtr` ringStartPtr
    len = assert (lenInBytes `mod` elemSize == 0) $ lenInBytes `div` elemSize
    off = assert (offInBytes `mod` elemSize == 0) $ offInBytes `div` elemSize
    advanceFromHead = (off + by `mod` len) * elemSize

-- | Insert an item at the head of the ring, when the ring is full this
-- replaces the oldest item in the ring with the new item. This is unsafe
-- because ringHead supplied is not verified to be within the Ring. Also,
-- the ringStart foreignPtr must be guaranteed to be alive by the caller.
{-# INLINE unsafeInsert #-}
unsafeInsert :: Storable a => Ring a -> Ptr a -> a -> IO (Ptr a)
unsafeInsert rb ringHead newVal = do
    poke ringHead newVal
    -- touchForeignPtr (ringStart rb)
    return $ advance rb ringHead

-- XXX remove all usage of A.unsafeInlineIO
--
-- | Like 'unsafeEqArray' but compares only N bytes instead of entire length of
-- the ring buffer. This is unsafe because the ringHead Ptr is not checked to
-- be in range.
{-# INLINE unsafeEqArrayN #-}
unsafeEqArrayN :: Ring a -> Ptr a -> A.Array a -> Int -> Bool
unsafeEqArrayN Ring{..} rh A.Array{..} n =
    let !res = A.unsafeInlineIO $ do
            let rs = unsafeForeignPtrToPtr ringStart
                as = unsafeForeignPtrToPtr aStart
            assert (aEnd `minusPtr` as >= ringBound `minusPtr` rs) (return ())
            let len = ringBound `minusPtr` rh
            r1 <- A.memcmp (castPtr rh) (castPtr as) (min len n)
            r2 <- if n > len
                then A.memcmp (castPtr rs) (castPtr (as `plusPtr` len))
                              (min (rh `minusPtr` rs) (n - len))
                else return True
            -- XXX enable these, check perf impact
            -- touchForeignPtr ringStart
            -- touchForeignPtr aStart
            return (r1 && r2)
    in res

-- | Byte compare the entire length of ringBuffer with the given array,
-- starting at the supplied ringHead pointer.  Returns true if the Array and
-- the ringBuffer have identical contents.
--
-- This is unsafe because the ringHead Ptr is not checked to be in range. The
-- supplied array must be equal to or bigger than the ringBuffer, ARRAY BOUNDS
-- ARE NOT CHECKED.
{-# INLINE unsafeEqArray #-}
unsafeEqArray :: Ring a -> Ptr a -> A.Array a -> Bool
unsafeEqArray Ring{..} rh A.Array{..} =
    let !res = A.unsafeInlineIO $ do
            let rs = unsafeForeignPtrToPtr ringStart
            let as = unsafeForeignPtrToPtr aStart
            assert (aEnd `minusPtr` as >= ringBound `minusPtr` rs)
                   (return ())
            let len = ringBound `minusPtr` rh
            r1 <- A.memcmp (castPtr rh) (castPtr as) len
            r2 <- A.memcmp (castPtr rs) (castPtr (as `plusPtr` len))
                           (rh `minusPtr` rs)
            -- XXX enable these, check perf impact
            -- touchForeignPtr ringStart
            -- touchForeignPtr aStart
            return (r1 && r2)
    in res

-- XXX use MonadIO
--
-- | Fold the buffer starting from ringStart up to the given 'Ptr' using a pure
-- step function. This is useful to fold the items in the ring when the ring is
-- not full. The supplied pointer is usually the end of the ring.
--
-- Unsafe because the supplied Ptr is not checked to be in range.
{-# INLINE unsafeFoldRing #-}
unsafeFoldRing :: forall a b. Storable a
    => Ptr a -> (b -> a -> b) -> b -> Ring a -> b
unsafeFoldRing ptr f z Ring{..} =
    let !res = A.unsafeInlineIO $ withForeignPtr ringStart $ \p ->
                    go z p ptr
    in res
    where
      go !acc !p !q
        | p == q = return acc
        | otherwise = do
            x <- peek p
            go (f acc x) (p `plusPtr` sizeOf (undefined :: a)) q

-- XXX Can we remove MonadIO here?
withForeignPtrM :: MonadIO m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtrM fp fn = do
    r <- fn $ unsafeForeignPtrToPtr fp
    liftIO $ touchForeignPtr fp
    return r

-- | Like unsafeFoldRing but with a monadic step function.
{-# INLINE unsafeFoldRingM #-}
unsafeFoldRingM :: forall m a b. (MonadIO m, Storable a)
    => Ptr a -> (b -> a -> m b) -> b -> Ring a -> m b
unsafeFoldRingM ptr f z Ring {..} =
    withForeignPtrM ringStart $ \x -> go z x ptr
  where
    go !acc !start !end
        | start == end = return acc
        | otherwise = do
            let !x = A.unsafeInlineIO $ peek start
            acc' <- f acc x
            go acc' (start `plusPtr` sizeOf (undefined :: a)) end

-- | Fold the entire length of a ring buffer starting at the supplied ringHead
-- pointer.  Assuming the supplied ringHead pointer points to the oldest item,
-- this would fold the ring starting from the oldest item to the newest item in
-- the ring.
--
-- Note, this will crash on ring of 0 size.
--
{-# INLINE unsafeFoldRingFullM #-}
unsafeFoldRingFullM :: forall m a b. (MonadIO m, Storable a)
    => Ptr a -> (b -> a -> m b) -> b -> Ring a -> m b
unsafeFoldRingFullM rh f z rb@Ring {..} =
    withForeignPtrM ringStart $ \_ -> go z rh
  where
    go !acc !start = do
        let !x = A.unsafeInlineIO $ peek start
        acc' <- f acc x
        let ptr = advance rb start
        if ptr == rh
            then return acc'
            else go acc' ptr

-- | Fold @Int@ items in the ring starting at @Ptr a@.  Won't fold more
-- than the length of the ring.
--
-- Note, this will crash on ring of 0 size.
--
{-# INLINE unsafeFoldRingNM #-}
unsafeFoldRingNM :: forall m a b. (MonadIO m, Storable a)
    => Int -> Ptr a -> (b -> a -> m b) -> b -> Ring a -> m b
unsafeFoldRingNM count rh f z rb@Ring {..} =
    withForeignPtrM ringStart $ \_ -> go count z rh

    where

    go 0 acc _ = return acc
    go !n !acc !start = do
        let !x = A.unsafeInlineIO $ peek start
        acc' <- f acc x
        let ptr = advance rb start
        if ptr == rh || n == 0
            then return acc'
            else go (n - 1) acc' ptr
