{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language TypeApplications #-}

import GHC.Exts
import GHC.IO (IO(..))
import GHC.Word (Word16(W16#))
import Control.Exception (SomeException,toException,try)
import GHC.IO.Exception (BoundsCheckException(BoundsCheckException))
import System.IO.Error (userError)
import Foreign.Storable (sizeOf)

data IntArray = IntArray ByteArray#
data MutableIntArray = MutableIntArray (MutableByteArray# RealWorld)

data Word16Array = Word16Array ByteArray#
data MutableWord16Array = MutableWord16Array (MutableByteArray# RealWorld)

main :: IO ()
main = do
  arrA <- newIntArray 5
  writeIntArray arrA 0 47
  writeIntArray arrA 1 49
  writeIntArray arrA 2 51
  writeIntArray arrA 3 53
  writeIntArray arrA 4 55
  demandException =<< try (readIntArray arrA (-1))
  demandException =<< try (readIntArray arrA 5)
  demandInt 47 =<< readIntArray arrA 0
  demandInt 55 =<< readIntArray arrA 4
  demandException =<< try (writeIntArray arrA 5 2467)
  demandException =<< try (writeIntArray arrA (-1) 1958)
  arrB <- newWord16Array 9
  writeWord16Array arrB 0 103
  writeWord16Array arrB 1 107
  writeWord16Array arrB 2 111
  writeWord16Array arrB 3 115
  writeWord16Array arrB 4 119
  writeWord16Array arrB 5 123
  writeWord16Array arrB 6 127
  writeWord16Array arrB 7 131
  writeWord16Array arrB 8 135
  demandException =<< try (readWord16Array arrB (-1))
  demandException =<< try (readWord16Array arrB 9)
  demandWord16 103 =<< readWord16Array arrB 0
  demandWord16 135 =<< readWord16Array arrB 8
  demandWord16 103 =<< readWord16ArrayUnaligned arrB 0
  demandWord16 135 =<< readWord16ArrayUnaligned arrB 16
  demandException =<< try (readWord16ArrayUnaligned arrB (-1))
  demandException =<< try (readWord16ArrayUnaligned arrB (-2))
  demandException =<< try (readWord16ArrayUnaligned arrB 17)
  demandException =<< try (readWord16ArrayUnaligned arrB 18)
  -- After testing the simple read/write operations on Word16Array,
  -- we use the more complicated range-copying functions.
  srcB <- unsafeFreezeWord16Array arrB
  dstB <- newWord16Array 13
  demandException =<< try (copyWord16Array dstB (-1) srcB 0 1)
  demandException =<< try (copyWord16Array dstB 0 srcB (-1) 1)
  demandException =<< try (copyWord16Array dstB 0 srcB 0 (-1))
  demandException =<< try (copyWord16Array dstB 0 srcB 8 2)
  demandException =<< try (copyWord16Array dstB 11 srcB 0 3)
  copyWord16Array dstB 0 srcB 0 9
  copyWord16Array dstB 4 srcB 0 9

demandException :: Either BoundsCheckException a -> IO ()
demandException (Left BoundsCheckException) = pure ()
demandException (Right _) = fail "expected exception"

demandInt :: Int -> Int -> IO ()
demandInt expected actual = if expected == actual
  then pure ()
  else fail $ "expected int " ++ show expected ++ " but got " ++ show actual

demandWord16 :: Word16 -> Word16 -> IO ()
demandWord16 expected actual = if expected == actual
  then pure ()
  else fail $ "expected word16 " ++ show expected ++ " but got " ++ show actual

sizeOfInt :: Int
sizeOfInt = sizeOf @Int undefined

unInt :: Int -> Int#
unInt (I# i) = i

newIntArray :: Int -> IO MutableIntArray
newIntArray (I# n#)
  = IO (\s# ->
      case newByteArray# (n# *# unInt sizeOfInt) s# of
        (# s'#, arr# #) -> (# s'#, MutableIntArray arr# #)
    )

newWord16Array :: Int -> IO MutableWord16Array
newWord16Array (I# n#)
  = IO (\s# ->
      case newByteArray# (n# *# 2#) s# of
        (# s'#, arr# #) -> (# s'#, MutableWord16Array arr# #)
    )

-- | Read a primitive value from the primitive array.
indexIntArray :: IntArray -> Int -> Int
indexIntArray (IntArray arr#) (I# i#) = I# (indexIntArray# arr# i#)

indexWord16Array :: Word16Array -> Int -> Word16
indexWord16Array (Word16Array arr#) (I# i#) = W16# (indexWord16Array# arr# i#)

readIntArray :: MutableIntArray -> Int -> IO Int
readIntArray (MutableIntArray arr#) (I# i#) = IO $ \s0 ->
  case readIntArray# arr# i# s0 of
    (# s1, r #) -> (# s1, I# r #)

readWord16Array :: MutableWord16Array -> Int -> IO Word16
readWord16Array (MutableWord16Array arr#) (I# i#) = IO $ \s0 ->
  case readWord16Array# arr# i# s0 of
    (# s1, r #) -> (# s1, W16# r #)

readWord16ArrayUnaligned ::
     MutableWord16Array
  -> Int -- index (in bytes, not elements)
  -> IO Word16
readWord16ArrayUnaligned (MutableWord16Array arr#) (I# i#) = IO $ \s0 ->
  case readWord8ArrayAsWord16# arr# i# s0 of
    (# s1, r #) -> (# s1, W16# r #)


writeIntArray ::
     MutableIntArray -- ^ array
  -> Int -- ^ index
  -> Int -- ^ element
  -> IO ()
writeIntArray (MutableIntArray arr#) (I# i#) (I# x) = IO $ \s0 ->
  case writeIntArray# arr# i# x s0 of
    s1 -> (# s1, () #)

writeWord16Array ::
     MutableWord16Array -- ^ array
  -> Int -- ^ index (in elements)
  -> Word16 -- ^ element
  -> IO ()
writeWord16Array (MutableWord16Array arr#) (I# i#) (W16# x) = IO $ \s0 ->
  case writeWord16Array# arr# i# x s0 of
    s1 -> (# s1, () #)

writeWord16ArrayUnaligned ::
     MutableWord16Array -- ^ array
  -> Int -- ^ index (in bytes)
  -> Word16 -- ^ element
  -> IO ()
writeWord16ArrayUnaligned (MutableWord16Array arr#) (I# i#) (W16# x) = IO $ \s0 ->
  case writeWord8ArrayAsWord16# arr# i# x s0 of
    s1 -> (# s1, () #)

unsafeFreezeIntArray :: MutableIntArray -> IO IntArray
unsafeFreezeIntArray (MutableIntArray arr#) = IO $ \s# ->
  case unsafeFreezeByteArray# arr# s# of
    (# s'#, arr'# #) -> (# s'#, IntArray arr'# #)

unsafeFreezeWord16Array :: MutableWord16Array -> IO Word16Array
unsafeFreezeWord16Array (MutableWord16Array arr#) = IO $ \s# ->
  case unsafeFreezeByteArray# arr# s# of
    (# s'#, arr'# #) -> (# s'#, Word16Array arr'# #)

copyWord16Array :: MutableWord16Array -> Int -> Word16Array -> Int -> Int -> IO ()
copyWord16Array (MutableWord16Array dst) (I# doff) (Word16Array src) (I# soff) (I# n)
  = IO $ \s0 -> case copyByteArray# src (soff *# 2#) dst (doff *# 2#) (n *# 2#) s0 of
    s1 -> (# s1, () #)
