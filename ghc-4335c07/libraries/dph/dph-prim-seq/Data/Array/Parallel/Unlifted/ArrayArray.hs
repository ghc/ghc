
-- | Irregular 2D unboxed arrays.
--
--   The difference between this type and something like 
--   @Data.Vector (Data.Vector.Unboxed a)@ is that the inner arrays have kind
--   @#@ and cannot be bottom. This ensures that we can always lookup an element
--   from an `ArrayArray#` without performing unboxings or checking for thunks.
---
--   TODO: move this into the Data.Primitive library.
module Data.Array.Parallel.Unlifted.ArrayArray
        ( MutableArrayArray (..)
        , ArrayArray(..)
        , newArrayArray
        , writeArrayArrayMut
        , writeArrayArray
        , readArrayArray
        , indexArrayArray
        , unsafeFreezeArrayArray
        , unsafeDeepFreezeArrayArray
        , copyArrayArray)
where	
import GHC.Prim
import GHC.Base
import GHC.ST
import Data.Primitive.ByteArray


data MutableArrayArray s e 
        = MutableArrayArray (MutableArrayArray# s)

data ArrayArray e 
        = ArrayArray ArrayArray#


-- | Create an `ArrayArray` with the given number of elements.
newArrayArray :: Int -> ST s (MutableArrayArray s e)
newArrayArray (I# n#)
  = ST $ \s# -> case newArrayArray# n# s# of
           (# s'#, arr# #) -> (# s'#, MutableArrayArray arr# #)
{-# INLINE newArrayArray #-}


-- | Write a `MutableByteArray` to an `MutableArrayArray`.
writeArrayArrayMut :: MutableArrayArray s (MutableByteArray s) -> Int -> MutableByteArray s -> ST s ()
writeArrayArrayMut (MutableArrayArray arrs#) (I# i#) (MutableByteArray mba#)
  = ST $ \s# -> case writeMutableByteArrayArray# arrs# i# mba# s# of
           s'# -> (# s'#, () #)
{-# INLINE writeArrayArrayMut #-}


-- | Write a `ByteArray` to a `MutableArrayArray`.
writeArrayArray :: MutableArrayArray s ByteArray -> Int -> ByteArray -> ST s ()
writeArrayArray (MutableArrayArray arrs#) (I# i#) (ByteArray ba#)
  = ST $ \s# -> case writeByteArrayArray# arrs# i# ba# s# of
           s'# -> (# s'#, () #)
{-# INLINE writeArrayArray #-}


-- | Read a `MutableByteArray` from a `MutableArrayArray`.
readArrayArray :: MutableArrayArray s (MutableByteArray s) -> Int -> ST s (MutableByteArray s)
readArrayArray (MutableArrayArray arrs#) (I# i#)
  = ST $ \s# -> case readMutableByteArrayArray# arrs# i# s# of
           (# s'#, mba# #) -> (# s'#, MutableByteArray mba# #)
{-# INLINE readArrayArray #-}


-- | Index an `ArrayArray` of `ByteArray`s.
indexArrayArray :: ArrayArray ByteArray -> Int -> ByteArray
indexArrayArray (ArrayArray arrs#) (I# i#)
  = ByteArray (indexByteArrayArray# arrs# i#)
{-# INLINE indexArrayArray #-}


-- | Freeze a `MutableArrayArray` into a plain `ArrayArray`.
unsafeFreezeArrayArray :: MutableArrayArray s e -> ST s (ArrayArray e)
unsafeFreezeArrayArray (MutableArrayArray marrs#)
  = ST $ \s# -> case unsafeFreezeArrayArray# marrs# s# of
           (# s'#, arrs# #)  -> (# s'#, ArrayArray arrs# #)
{-# INLINE unsafeFreezeArrayArray #-}


-- | Freeze a nested `MutableArrayArray` into an `ArrayArray`.
unsafeDeepFreezeArrayArray 
        :: forall s
        .  MutableArrayArray s (MutableByteArray s) 
        -> ST s (ArrayArray ByteArray)

unsafeDeepFreezeArrayArray marrs@(MutableArrayArray marrs#)
 = do   let n = I# (sizeofMutableArrayArray# marrs#)
            marrs_halfFrozen = MutableArrayArray marrs#  -- :: MutableArrayArray s (ByteArray e)
        mapM_ (freezeSubArray marrs_halfFrozen) [0..n - 1]
        unsafeFreezeArrayArray marrs_halfFrozen
       
  where
    freezeSubArray marrs_halfFrozen i
      = do  mba <- readArrayArray marrs i
            ba  <- unsafeFreezeByteArray mba
            writeArrayArray marrs_halfFrozen i ba
{-# INLINE unsafeDeepFreezeArrayArray #-}


-- | Copy an ArrayArray
copyArrayArray 
        :: MutableArrayArray s ByteArray -> Int
        -> ArrayArray ByteArray -> Int
        -> Int -> ST s ()

copyArrayArray dst startDst src startSrc len
 = loop startDst startSrc len
 where  loop !ixDst !ixSrc !len'
         | len' <= 0     = return ()
         | otherwise
         = do   writeArrayArray dst ixDst $ indexArrayArray src ixSrc
                loop (ixDst + 1) (ixSrc + 1) (len' - 1)

