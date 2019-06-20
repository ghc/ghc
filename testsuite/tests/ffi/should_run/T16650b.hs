{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language UnliftedFFITypes #-}
{-# language ForeignFunctionInterface #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

-- Test for shims when passing an array of unlifted values
-- to a foreign function.
-- See test T16650a for more commentary.

import GHC.Exts
import GHC.Word
import GHC.IO
import Data.Kind (Type)

main :: IO ()
main = do
  mb0 <- luckySingleton
  mb1 <- luckySingleton
  mbs <- newByteArrays 2
  writeByteArrays mbs 0 mb0
  writeByteArrays mbs 1 mb0
  case box mbs of
    Box x -> print =<< c_is_doubleton_homogeneous (unsafeCoerce# x)
  writeByteArrays mbs 1 mb1
  case box mbs of
    Box x -> print =<< c_is_doubleton_homogeneous (unsafeCoerce# x)

foreign import ccall unsafe "is_doubleton_homogenous"
  c_is_doubleton_homogeneous :: MutableArrayArray# RealWorld -> IO Word8

data Box :: Type where
  Box :: (Any :: TYPE 'UnliftedRep) -> Box

-- An array of bytes
data MutableByteArray :: Type where
  MutableByteArray :: MutableByteArray# RealWorld -> MutableByteArray

-- A mutable array of mutable byte arrays
data MutableByteArrays :: Type where
  MutableByteArrays :: MutableArrayArray# RealWorld -> MutableByteArrays

box :: MutableByteArrays -> Box
{-# noinline box #-}
box (MutableByteArrays x) = Box (unsafeCoerce# x)

luckySingleton :: IO MutableByteArray
luckySingleton = IO $ \s0 -> case newByteArray# 1# s0 of
  (# s1, marr# #) -> case writeWord8Array# marr# 0# 42## s1 of
    s2 -> (# s2, MutableByteArray marr# #)

readByteArray :: MutableByteArray -> Int -> IO Word8
readByteArray (MutableByteArray b#) (I# i#) = IO $ \s0 ->
  case readWord8Array# b# i# s0 of
    (# s1, w #) -> (# s1, W8# w #)

-- Write a mutable byte array to the array of mutable byte arrays
-- at the given index.
writeByteArrays :: MutableByteArrays -> Int -> MutableByteArray -> IO ()
writeByteArrays (MutableByteArrays maa#) (I# i#) (MutableByteArray a) = IO $ \s0 ->
  case writeMutableByteArrayArray# maa# i# a s0 of
    s1 -> (# s1, () #)

-- Allocate a new array of mutable byte arrays. All elements are
-- uninitialized. Attempting to read them will cause a crash.
newByteArrays :: Int -> IO MutableByteArrays
newByteArrays (I# len#) = IO $ \s0 -> case newArrayArray# len# s0 of
  (# s1, a# #) -> (# s1, MutableByteArrays a# #)
