{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, FlexibleContexts #-}
-- | Efficient serialisation for GHCi Instruction arrays
--
-- Author: Ben Gamari
--
module GHCi.BinaryArray(putArray, getArray) where

import Prelude
import Foreign.Ptr
import Data.Binary
import Data.Binary.Put (putBuilder)
import qualified Data.Binary.Get.Internal as Binary
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Internal as BB
import qualified Data.Array.Base as A
import qualified Data.Array.IO.Internals as A
import qualified Data.Array.Unboxed as A
import GHC.Exts
import GHC.IO

-- | An efficient serialiser of 'A.UArray'.
putArray :: Binary i => A.UArray i a -> Put
putArray (A.UArray l u _ arr#) = do
    put l
    put u
    putBuilder $ byteArrayBuilder arr#

byteArrayBuilder :: ByteArray# -> BB.Builder
byteArrayBuilder arr# = BB.builder $ go 0 (I# (sizeofByteArray# arr#))
  where
    go :: Int -> Int -> BB.BuildStep a -> BB.BuildStep a
    go !inStart !inEnd k (BB.BufferRange outStart outEnd)
      -- There is enough room in this output buffer to write all remaining array
      -- contents
      | inRemaining <= outRemaining = do
          copyByteArrayToAddr arr# inStart outStart inRemaining
          k (BB.BufferRange (outStart `plusPtr` inRemaining) outEnd)
      -- There is only enough space for a fraction of the remaining contents
      | otherwise = do
          copyByteArrayToAddr arr# inStart outStart outRemaining
          let !inStart' = inStart + outRemaining
          return $! BB.bufferFull 1 outEnd (go inStart' inEnd k)
      where
        inRemaining  = inEnd - inStart
        outRemaining = outEnd `minusPtr` outStart

    copyByteArrayToAddr :: ByteArray# -> Int -> Ptr a -> Int -> IO ()
    copyByteArrayToAddr src# (I# src_off#) (Ptr dst#) (I# len#) =
        IO $ \s -> case copyByteArrayToAddr# src# src_off# dst# len# s of
                     s' -> (# s', () #)

-- | An efficient deserialiser of 'A.UArray'.
getArray :: (Binary i, A.Ix i, A.MArray A.IOUArray a IO) => Get (A.UArray i a)
getArray = do
    l <- get
    u <- get
    arr@(A.IOUArray (A.STUArray _ _ _ arr#)) <-
        return $ unsafeDupablePerformIO $ A.newArray_ (l,u)
    let go 0 _ = return ()
        go !remaining !off = do
            Binary.readNWith n $ \ptr ->
              copyAddrToByteArray ptr arr# off n
            go (remaining - n) (off + n)
          where n = min chunkSize remaining
    go (I# (sizeofMutableByteArray# arr#)) 0
    return $! unsafeDupablePerformIO $ unsafeFreezeIOUArray arr
  where
    chunkSize = 10*1024

    copyAddrToByteArray :: Ptr a -> MutableByteArray# RealWorld
                        -> Int -> Int -> IO ()
    copyAddrToByteArray (Ptr src#) dst# (I# dst_off#) (I# len#) =
        IO $ \s -> case copyAddrToByteArray# src# dst# dst_off# len# s of
                     s' -> (# s', () #)

-- this is inexplicably not exported in currently released array versions
unsafeFreezeIOUArray :: A.IOUArray ix e -> IO (A.UArray ix e)
unsafeFreezeIOUArray (A.IOUArray marr) = stToIO (A.unsafeFreezeSTUArray marr)
