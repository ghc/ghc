{-# language ForeignFunctionInterface #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language UnliftedFFITypes #-}
{-# language UnliftedNewtypes #-}

{-# OPTIONS_GHC -O2 #-}

import Data.Kind (Type)
import Data.Word
import GHC.Exts
import GHC.IO
import GHC.Word

foreign import ccall unsafe "head_bytearray"
  c_head_bytearray_a :: MutableByteArray# RealWorld -> IO Word8
foreign import ccall unsafe "head_bytearray"
  c_head_bytearray_b :: MyArray# -> IO Word8

newtype MyArray# :: TYPE 'UnliftedRep where
  MyArray# :: MutableByteArray# RealWorld -> MyArray#

data MutableByteArray :: Type where
  MutableByteArray :: MutableByteArray# RealWorld -> MutableByteArray

main :: IO ()
main = do
  ba@(MutableByteArray ba#) <- luckySingleton
  print =<< readByteArray ba 0
  print =<< c_head_bytearray_a ba#
  print =<< c_head_bytearray_b (MyArray# ba#)

readByteArray :: MutableByteArray -> Int -> IO Word8
readByteArray (MutableByteArray b#) (I# i#) = IO $ \s0 ->
  case readWord8Array# b# i# s0 of
    (# s1, w #) -> (# s1, W8# w #)

-- Create a new mutable byte array of length 1 with the sole byte
-- set to the 105.
luckySingleton :: IO MutableByteArray
luckySingleton = IO $ \s0 -> case newByteArray# 1# s0 of
  (# s1, marr# #) -> case writeWord8Array# marr# 0# 105## s1 of
    s2 -> (# s2, MutableByteArray marr# #)


