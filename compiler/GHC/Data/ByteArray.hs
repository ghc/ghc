{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module GHC.Data.ByteArray
  ( -- * Immutable byte arrays
    ByteArray
  , getByteArray
  , unsafeByteArrayContents
  , withByteArrayContents
  , sizeofByteArray

    -- * Mutable byte arrays
  , MutableByteArray
  , getMutableByteArray
  , unsafeMutableByteArrayContents
  , newMutableByteArray
  , newPinnedMutableByteArray
  , copyByteArray
  , copyAddrToMutableByteArray
  , unsafeFreezeByteArray

    -- * Writing
  , writeWordArray
  , writeWord8Array
  , writeWord16Array
  , writeWord32Array
  , writeWord64Array
  , writeIntArray
  , writeInt8Array
  , writeInt16Array
  , writeInt32Array
  , writeInt64Array
  , writeCharArray

    -- * Reading
  , readWordArray
  , readWord8Array
  , readWord16Array
  , readWord32Array
  , readWord64Array
  , readIntArray
  , readInt8Array
  , readInt16Array
  , readInt32Array
  , readInt64Array
  , readCharArray

    -- * Immutable indexing
  , indexWordArray
  , indexWord8Array
  , indexWord16Array
  , indexWord32Array
  , indexWord64Array
  , indexIntArray
  , indexInt8Array
  , indexInt16Array
  , indexInt32Array
  , indexInt64Array
  , indexCharArray
  ) where

import GHC.Base
import GHC.Exts
import GHC.Word
import GHC.Int
import Unsafe.Coerce

data MutableByteArray = MutableByteArray { getMutableByteArray :: !(MutableByteArray# RealWorld) }

data ByteArray = ByteArray { getByteArray :: !ByteArray# }

unsafeByteArrayContents :: ByteArray -> Ptr a
unsafeByteArrayContents (ByteArray ba) = Ptr (byteArrayContents# ba)

unsafeMutableByteArrayContents :: MutableByteArray -> Ptr a
unsafeMutableByteArrayContents = unsafeByteArrayContents . unsafeCoerce

withByteArrayContents :: ByteArray -> (Ptr a -> IO b) -> IO b
withByteArrayContents (ByteArray ba) f = do
  r <- f $ Ptr (byteArrayContents# ba)
  IO $ \s -> case touch# ba s of s' -> (# s', () #)
  return r

newMutableByteArray :: Int -> IO MutableByteArray
newMutableByteArray (I# size) = IO $ \s ->
  case newByteArray# size s of
    (# s', mba #) -> (# s', MutableByteArray mba #)

newPinnedMutableByteArray :: Int -> IO MutableByteArray
newPinnedMutableByteArray (I# size) = IO $ \s ->
  case newPinnedByteArray# size s of
    (# s', mba #) -> (# s', MutableByteArray mba #)

copyByteArray
  :: ByteArray          -- ^ source
  -> Int                -- ^ source offset
  -> MutableByteArray   -- ^ destination
  -> Int                -- ^ destination offset
  -> Int                -- ^ length to copy
  -> IO ()
copyByteArray (ByteArray src) (I# src_ofs) (MutableByteArray dst) (I# dst_ofs) (I# len) =
  IO $ \s ->
    case copyByteArray# src src_ofs dst dst_ofs len s of
      s' -> (# s', () #)

copyAddrToMutableByteArray :: Ptr a -> MutableByteArray -> Int -> Int -> IO ()
copyAddrToMutableByteArray (Ptr src) (MutableByteArray dst) (I# dst_ofs) (I# len) = IO $ \s ->
  case copyAddrToByteArray# src dst dst_ofs len s of
    s' -> (# s', () #)

unsafeFreezeByteArray
  :: MutableByteArray
  -> IO ByteArray
unsafeFreezeByteArray (MutableByteArray mba) = IO $ \s ->
  case unsafeFreezeByteArray# mba s of
    (# s', ba #) -> (# s', ByteArray ba #)

sizeofByteArray :: ByteArray -> Int
sizeofByteArray (ByteArray arr) = I# (sizeofByteArray# arr)


readWordArray :: MutableByteArray -> Int -> IO Word
readWordArray (MutableByteArray arr) (I# ix) = IO $ \s0 ->
    case readWordArray# arr ix s0 of
      (# s1, r #) -> (# s1, W# r #)

readWord8Array :: MutableByteArray -> Int -> IO Word8
readWord8Array (MutableByteArray arr) (I# ix) = IO $ \s0 ->
    case readWord8Array# arr ix s0 of
      (# s1, r #) -> (# s1, W8# r #)

readWord16Array :: MutableByteArray -> Int -> IO Word16
readWord16Array (MutableByteArray arr) (I# ix) = IO $ \s0 ->
    case readWord16Array# arr ix s0 of
      (# s1, r #) -> (# s1, W16# r #)

readWord32Array :: MutableByteArray -> Int -> IO Word32
readWord32Array (MutableByteArray arr) (I# ix) = IO $ \s0 ->
    case readWord32Array# arr ix s0 of
      (# s1, r #) -> (# s1, W32# r #)

readWord64Array :: MutableByteArray -> Int -> IO Word64
readWord64Array (MutableByteArray arr) (I# ix) = IO $ \s0 ->
    case readWord64Array# arr ix s0 of
      (# s1, r #) -> (# s1, W64# r #)

readIntArray :: MutableByteArray -> Int -> IO Int
readIntArray (MutableByteArray arr) (I# ix) = IO $ \s0 ->
    case readIntArray# arr ix s0 of
      (# s1, r #) -> (# s1, I# r #)

readInt8Array :: MutableByteArray -> Int -> IO Int8
readInt8Array (MutableByteArray arr) (I# ix) = IO $ \s0 ->
    case readInt8Array# arr ix s0 of
      (# s1, r #) -> (# s1, I8# r #)

readInt16Array :: MutableByteArray -> Int -> IO Int16
readInt16Array (MutableByteArray arr) (I# ix) = IO $ \s0 ->
    case readInt16Array# arr ix s0 of
      (# s1, r #) -> (# s1, I16# r #)

readInt32Array :: MutableByteArray -> Int -> IO Int32
readInt32Array (MutableByteArray arr) (I# ix) = IO $ \s0 ->
    case readInt32Array# arr ix s0 of
      (# s1, r #) -> (# s1, I32# r #)

readInt64Array :: MutableByteArray -> Int -> IO Int64
readInt64Array (MutableByteArray arr) (I# ix) = IO $ \s0 ->
    case readInt64Array# arr ix s0 of
      (# s1, r #) -> (# s1, I64# r #)

readCharArray :: MutableByteArray -> Int -> IO Char
readCharArray (MutableByteArray arr) (I# ix) = IO $ \s0 ->
    case readCharArray# arr ix s0 of
      (# s1, r #) -> (# s1, C# r #)



writeWordArray :: MutableByteArray -> Int -> Word -> IO ()
writeWordArray (MutableByteArray arr) (I# ix) (W# x) = IO $ \s0 ->
    case writeWordArray# arr ix x s0 of
      s1 -> (# s1, () #)

writeWord8Array :: MutableByteArray -> Int -> Word8 -> IO ()
writeWord8Array (MutableByteArray arr) (I# ix) (W8# x) = IO $ \s0 ->
    case writeWord8Array# arr ix x s0 of
      s1 -> (# s1, () #)

writeWord16Array :: MutableByteArray -> Int -> Word16 -> IO ()
writeWord16Array (MutableByteArray arr) (I# ix) (W16# x) = IO $ \s0 ->
    case writeWord16Array# arr ix x s0 of
      s1 -> (# s1, () #)

writeWord32Array :: MutableByteArray -> Int -> Word32 -> IO ()
writeWord32Array (MutableByteArray arr) (I# ix) (W32# x) = IO $ \s0 ->
    case writeWord32Array# arr ix x s0 of
      s1 -> (# s1, () #)

writeWord64Array :: MutableByteArray -> Int -> Word64 -> IO ()
writeWord64Array (MutableByteArray arr) (I# ix) (W64# x) = IO $ \s0 ->
    case writeWord64Array# arr ix x s0 of
      s1 -> (# s1, () #)

writeIntArray :: MutableByteArray -> Int -> Int -> IO ()
writeIntArray (MutableByteArray arr) (I# ix) (I# x) = IO $ \s0 ->
    case writeIntArray# arr ix x s0 of
      s1 -> (# s1, () #)

writeInt8Array :: MutableByteArray -> Int -> Int8 -> IO ()
writeInt8Array (MutableByteArray arr) (I# ix) (I8# x) = IO $ \s0 ->
    case writeInt8Array# arr ix x s0 of
      s1 -> (# s1, () #)

writeInt16Array :: MutableByteArray -> Int -> Int16 -> IO ()
writeInt16Array (MutableByteArray arr) (I# ix) (I16# x) = IO $ \s0 ->
    case writeInt16Array# arr ix x s0 of
      s1 -> (# s1, () #)

writeInt32Array :: MutableByteArray -> Int -> Int32 -> IO ()
writeInt32Array (MutableByteArray arr) (I# ix) (I32# x) = IO $ \s0 ->
    case writeInt32Array# arr ix x s0 of
      s1 -> (# s1, () #)

writeInt64Array :: MutableByteArray -> Int -> Int64 -> IO ()
writeInt64Array (MutableByteArray arr) (I# ix) (I64# x) = IO $ \s0 ->
    case writeInt64Array# arr ix x s0 of
      s1 -> (# s1, () #)

writeCharArray :: MutableByteArray -> Int -> Char -> IO ()
writeCharArray (MutableByteArray arr) (I# ix) (C# x) = IO $ \s0 ->
    case writeCharArray# arr ix x s0 of
      s1 -> (# s1, () #)



indexWordArray :: ByteArray -> Int -> Word
indexWordArray (ByteArray arr) (I# ix) =
    W# (indexWordArray# arr ix)

indexWord8Array :: ByteArray -> Int -> Word8
indexWord8Array (ByteArray arr) (I# ix) =
    W8# (indexWord8Array# arr ix)

indexWord16Array :: ByteArray -> Int -> Word16
indexWord16Array (ByteArray arr) (I# ix) =
    W16# (indexWord16Array# arr ix)

indexWord32Array :: ByteArray -> Int -> Word32
indexWord32Array (ByteArray arr) (I# ix) =
    W32# (indexWord32Array# arr ix)

indexWord64Array :: ByteArray -> Int -> Word64
indexWord64Array (ByteArray arr) (I# ix) =
    W64# (indexWord64Array# arr ix)

indexIntArray :: ByteArray -> Int -> Int
indexIntArray (ByteArray arr) (I# ix) =
    I# (indexIntArray# arr ix)

indexInt8Array :: ByteArray -> Int -> Int8
indexInt8Array (ByteArray arr) (I# ix) =
    I8# (indexInt8Array# arr ix)

indexInt16Array :: ByteArray -> Int -> Int16
indexInt16Array (ByteArray arr) (I# ix) =
    I16# (indexInt16Array# arr ix)

indexInt32Array :: ByteArray -> Int -> Int32
indexInt32Array (ByteArray arr) (I# ix) =
    I32# (indexInt32Array# arr ix)

indexInt64Array :: ByteArray -> Int -> Int64
indexInt64Array (ByteArray arr) (I# ix) =
    I64# (indexInt64Array# arr ix)

indexCharArray :: ByteArray -> Int -> Char
indexCharArray (ByteArray arr) (I# ix) =
    C# (indexCharArray# arr ix)

