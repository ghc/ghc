{-# LANGUAGE MagicHash, UnboxedTuples, ScopedTypeVariables #-}

module Main where

import GHC.Prim
import GHC.Base
import GHC.ST
import GHC.Word
import Control.Monad
import System.Mem

data MutableByteArray s = MutableByteArray (MutableByteArray# s)

data ByteArray e = ByteArray ByteArray#

newByteArray :: Int -> ST s (MutableByteArray s)
newByteArray (I# n#)
  = ST $ \s# -> case newByteArray# n# s# of
           (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #)

writeByteArray :: MutableByteArray s -> Int -> Word32 -> ST s ()
writeByteArray (MutableByteArray mba#) (I# i#) (W32# w#)
  = ST $ \s# -> case writeWord32Array# mba# i# w# s# of
           s'# -> (# s'#, () #)

indexArray :: ByteArray Word32 -> Int -> Word32
indexArray (ByteArray arr#) (I# i#)
  = W32# (indexWord32Array# arr# i#)

unsafeFreezeByteArray :: MutableByteArray s -> ST s (ByteArray e)
unsafeFreezeByteArray (MutableByteArray mba#)
  = ST $ \s# -> case unsafeFreezeByteArray# mba# s# of
           (# s'#, ba# #)  -> (# s'#, ByteArray ba# #)

data MutableArrayArray s e = MutableArrayArray (MutableArrayArray# s)

data ArrayArray e = ArrayArray ArrayArray#

newArrayArray :: Int -> ST s (MutableArrayArray s e)
newArrayArray (I# n#)
  = ST $ \s# -> case newArrayArray# n# s# of
           (# s'#, arr# #) -> (# s'#, MutableArrayArray arr# #)

writeArrayArrayMut :: MutableArrayArray s (MutableByteArray s) -> Int -> MutableByteArray s
                -> ST s ()
writeArrayArrayMut (MutableArrayArray arrs#) (I# i#) (MutableByteArray mba#)
  = ST $ \s# -> case writeMutableByteArrayArray# arrs# i# mba# s# of
           s'# -> (# s'#, () #)

writeArrayArray :: MutableArrayArray s (ByteArray s) -> Int -> ByteArray s
                -> ST s ()
writeArrayArray (MutableArrayArray arrs#) (I# i#) (ByteArray ba#)
  = ST $ \s# -> case writeByteArrayArray# arrs# i# ba# s# of
           s'# -> (# s'#, () #)

readArrayArray :: MutableArrayArray s (MutableByteArray s) -> Int -> ST s (MutableByteArray s)
readArrayArray (MutableArrayArray arrs#) (I# i#)
  = ST $ \s# -> case readMutableByteArrayArray# arrs# i# s# of
           (# s'#, mba# #) -> (# s'#, MutableByteArray mba# #)

indexArrayArray :: ArrayArray (ByteArray e) -> Int -> ByteArray e
indexArrayArray (ArrayArray arrs#) (I# i#)
  = ByteArray (indexByteArrayArray# arrs# i#)

unsafeFreezeArrayArray :: MutableArrayArray s e -> ST s (ArrayArray e)
unsafeFreezeArrayArray (MutableArrayArray marrs#)
  = ST $ \s# -> case unsafeFreezeArrayArray# marrs# s# of
           (# s'#, arrs# #)  -> (# s'#, ArrayArray arrs# #)

unsafeDeepFreezeArrayArray :: forall s e
                           .  MutableArrayArray s (MutableByteArray s) 
                           -> ST s (ArrayArray (ByteArray e))
unsafeDeepFreezeArrayArray marrs@(MutableArrayArray marrs#)
  = do { let n = I# (sizeofMutableArrayArray# marrs#)
             marrs_halfFrozen = MutableArrayArray marrs#  -- :: MutableArrayArray s (ByteArray e)
       ; mapM_ (freezeSubArray marrs_halfFrozen) [0..n - 1]
       ; unsafeFreezeArrayArray marrs_halfFrozen
       }
  where
    freezeSubArray marrs_halfFrozen i
      = do { mba <- readArrayArray marrs i
           ; ba  <- unsafeFreezeByteArray mba
           ; writeArrayArray marrs_halfFrozen i ba
           }

newByteArrays :: [Int] -> ST s (MutableArrayArray s (MutableByteArray s))
newByteArrays ns
  = do { arrs <- newArrayArray (length ns)
       ; zipWithM_ (writeNewByteArray arrs) ns [0..]
       ; return arrs
       }
  where
    writeNewByteArray arrs n i
      = do { mba <- newByteArray (n * 4)    -- we store 32-bit words
           ; writeArrayArrayMut arrs i mba
           }

type UnboxedArray2D e = ArrayArray (ByteArray e)

newUnboxedArray2D :: [[Word32]] -> UnboxedArray2D Word32
newUnboxedArray2D values
  = runST $
    do { marrs <- newByteArrays (map length values)
       ; zipWithM_ (fill marrs) values [0..]
       ; arrs <- unsafeDeepFreezeArrayArray marrs
       ; return arrs
       }
  where
    fill marrs vs i
      = do { mba <- readArrayArray marrs i
           ; zipWithM_ (writeByteArray mba) [0..] vs
           }

unboxedArray2D :: UnboxedArray2D Word32
unboxedArray2D 
  = newUnboxedArray2D
    [ [1..10]
    , [11..200]
    , []
    , [1..1000] ++ [42] ++ [1001..2000]
    , [1..100000]
    ]

indexUnboxedArray2D :: UnboxedArray2D Word32 -> (Int, Int) -> Word32
indexUnboxedArray2D arr (i, j)
  = indexArrayArray arr i `indexArray` j

main 
  = do { print $ unboxedArray2D `indexUnboxedArray2D` (3, 1000)
       ; performGC
       ; print $ unboxedArray2D `indexUnboxedArray2D` (3, 1000)
       }
