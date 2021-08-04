-- |
-- Module      : Basement.UArray.Mutable -- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A simple array abstraction that allow to use typed
-- array of bytes where the array is pinned in memory
-- to allow easy use with Foreign interfaces, ByteString
-- and always aligned to 64 bytes.
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Basement.UArray.Mutable
    ( MUArray(..)
    -- * Property queries
    , sizeInMutableBytesOfContent
    , mutableLength
    , mutableOffset
    , mutableSame
    , onMutableBackend
    -- * Allocation & Copy
    , new
    , newPinned
    , newNative
    , newNative_
    , mutableForeignMem
    , copyAt
    , copyFromPtr
    , copyToPtr
    , sub
    -- , copyAddr
    -- * Reading and Writing cells
    , unsafeWrite
    , unsafeRead
    , write
    , read
    , withMutablePtr
    , withMutablePtrHint
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.Ptr
import           Basement.Compat.Base
import           Basement.Compat.Primitive
import           Data.Proxy
import           Basement.Types.OffsetSize
import           Basement.Monad
import           Basement.PrimType
import           Basement.FinalPtr
import           Basement.Exception
import qualified Basement.Block         as BLK
import qualified Basement.Block.Mutable as MBLK
import           Basement.Block         (MutableBlock(..))
import           Basement.UArray.Base hiding (empty)
import           Basement.Numerical.Subtractive
import           Foreign.Marshal.Utils (copyBytes)

sizeInMutableBytesOfContent :: forall ty s . PrimType ty => MUArray ty s -> CountOf Word8
sizeInMutableBytesOfContent _ = primSizeInBytes (Proxy :: Proxy ty)
{-# INLINE sizeInMutableBytesOfContent #-}

-- | read a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
read :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Offset ty -> prim ty
read array n
    | isOutOfBound n len = primOutOfBound OOB_Read n len
    | otherwise          = unsafeRead array n
  where len = mutableLength array
{-# INLINE read #-}

-- | Write to a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
write :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Offset ty -> ty -> prim ()
write array n val
    | isOutOfBound n len = primOutOfBound OOB_Write n len
    | otherwise          = unsafeWrite array n val
  where
    len = mutableLength array
{-# INLINE write #-}

empty :: (PrimType ty, PrimMonad prim) => prim (MUArray ty (PrimState prim))
empty = MUArray 0 0 . MUArrayMBA <$> MBLK.mutableEmpty

mutableSame :: MUArray ty st -> MUArray ty st -> Bool
mutableSame (MUArray sa ea (MUArrayMBA (MutableBlock ma))) (MUArray sb eb (MUArrayMBA (MutableBlock mb))) = (sa == sb) && (ea == eb) && bool# (sameMutableByteArray# ma mb)
mutableSame (MUArray s1 e1 (MUArrayAddr f1)) (MUArray s2 e2 (MUArrayAddr f2)) = (s1 == s2) && (e1 == e2) && finalPtrSameMemory f1 f2
mutableSame _ _ = False

mutableForeignMem :: (PrimMonad prim, PrimType ty)
                  => FinalPtr ty -- ^ the start pointer with a finalizer
                  -> Int         -- ^ the number of elements (in elements, not bytes)
                  -> prim (MUArray ty (PrimState prim))
mutableForeignMem fptr nb = pure $ MUArray (Offset 0) (CountOf nb) (MUArrayAddr fptr)

sub :: (PrimMonad prim, PrimType ty)
    => MUArray ty (PrimState prim)
    -> Int -- The number of elements to drop ahead
    -> Int -- Then the number of element to retain
    -> prim (MUArray ty (PrimState prim))
sub (MUArray start sz back) dropElems' takeElems
    | takeElems <= 0 = empty
    | Just keepElems <- sz - dropElems, keepElems > 0 
                     = pure $ MUArray (start `offsetPlusE` dropElems) (min (CountOf takeElems) keepElems) back
    | otherwise      = empty
  where
    dropElems = max 0 (CountOf dropElems')


-- | return the numbers of elements in a mutable array
mutableLength :: PrimType ty => MUArray ty st -> CountOf ty
mutableLength (MUArray _ end _)   = end

withMutablePtrHint :: forall ty prim a . (PrimMonad prim, PrimType ty)
                   => Bool
                   -> Bool
                   -> MUArray ty (PrimState prim)
                   -> (Ptr ty -> prim a)
                   -> prim a
withMutablePtrHint skipCopy skipCopyBack (MUArray start _ back) f =
    case back of
        MUArrayAddr fptr -> withFinalPtr fptr (\ptr -> f (ptr `plusPtr` os))
        MUArrayMBA mb    -> MBLK.withMutablePtrHint skipCopy skipCopyBack mb $ \ptr -> f (ptr `plusPtr` os)
  where
    sz           = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset os) = offsetOfE sz start

-- | Create a pointer on the beginning of the mutable array
-- and call a function 'f'.
--
-- The mutable buffer can be mutated by the 'f' function
-- and the change will be reflected in the mutable array
--
-- If the mutable array is unpinned, a trampoline buffer
-- is created and the data is only copied when 'f' return.
withMutablePtr :: (PrimMonad prim, PrimType ty)
               => MUArray ty (PrimState prim)
               -> (Ptr ty -> prim a)
               -> prim a
withMutablePtr = withMutablePtrHint False False

-- | Copy from a pointer, @count@ elements, into the mutable array
copyFromPtr :: forall prim ty . (PrimMonad prim, PrimType ty)
            => Ptr ty -> CountOf ty -> MUArray ty (PrimState prim) -> prim ()
copyFromPtr src@(Ptr src#) count marr
    | count > arrSz = primOutOfBound OOB_MemCopy (sizeAsOffset count) arrSz
    | otherwise     = onMutableBackend copyNative copyPtr marr
  where
    arrSz = mutableLength marr
    ofs = mutableOffset marr

    sz = primSizeInBytes (Proxy :: Proxy ty)
    !count'@(CountOf bytes@(I# bytes#)) = sizeOfE sz count
    !off'@(Offset od@(I# od#)) = offsetOfE sz ofs

    copyNative mba = MBLK.unsafeCopyBytesPtr mba off' src count'
    copyPtr fptr = withFinalPtr fptr $ \dst ->
        unsafePrimFromIO $ copyBytes (dst `plusPtr` od) src bytes

-- | Copy all the block content to the memory starting at the destination address
copyToPtr :: forall ty prim . (PrimType ty, PrimMonad prim)
          => MUArray ty (PrimState prim) -- ^ the source mutable array to copy
          -> Ptr ty                      -- ^ The destination address where the copy is going to start
          -> prim ()
copyToPtr marr dst@(Ptr dst#) = onMutableBackend copyNative copyPtr marr
  where
    copyNative (MutableBlock mba) = primitive $ \s1 ->
        case unsafeFreezeByteArray# mba s1 of
            (# s2, ba #) -> (# copyByteArrayToAddr# ba os# dst# szBytes# s2, () #)
    copyPtr fptr = unsafePrimFromIO $ withFinalPtr fptr $ \ptr ->
        copyBytes dst (ptr `plusPtr` os) szBytes

    !(Offset os@(I# os#)) = offsetInBytes $ mutableOffset marr
    !(CountOf szBytes@(I# szBytes#)) = sizeInBytes $ mutableLength marr

mutableOffset :: MUArray ty st -> Offset ty
mutableOffset (MUArray ofs _ _) = ofs
