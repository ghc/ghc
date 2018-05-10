{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}

module Packed.Bytes
  ( Bytes(..)
  , pack
  , unpack
  , length
    -- * Folds
  , foldr
    -- * Unsliced Byte Arrays
  , fromByteArray
  ) where

import Prelude hiding (take,length,replicate,drop,null,concat,foldr)

import Data.Primitive (ByteArray(..))
import Data.Word (Word8)
import Control.Monad.ST (runST, ST)
import qualified Data.Primitive as PM
import qualified GHC.OldList as L

data Bytes = Bytes
  {-# UNPACK #-} !ByteArray -- payload
  {-# UNPACK #-} !Int -- offset
  {-# UNPACK #-} !Int -- length

instance Show Bytes where
  show x = "pack " ++ show (unpack x)

pack :: [Word8] -> Bytes
pack bs = let arr = packByteArray bs in Bytes arr 0 (lengthByteArray arr)

unpack :: Bytes -> [Word8]
unpack (Bytes arr off len) = go off
  where
  go :: Int -> [Word8]
  go !ix = if ix < len + off
    then PM.indexByteArray arr ix : go (ix + 1)
    else []

fromByteArray :: ByteArray -> Bytes
fromByteArray ba = Bytes ba 0 (lengthByteArray ba)

length :: Bytes -> Int
length (Bytes _ _ len) = len

foldr :: (Word8 -> a -> a) -> a -> Bytes -> a
foldr f a0 (Bytes arr off0 len) = go off0 where
  !end = off0 + len
  go !ix = if ix < end
    then f (PM.indexByteArray arr ix) (go (ix + 1))
    else a0

packByteArray :: [Word8] -> ByteArray
packByteArray ws0 = runST $ do
  marr <- PM.newByteArray (L.length ws0)
  let go [] !_ = return ()
      go (w : ws) !ix = PM.writeByteArray marr ix w >> go ws (ix + 1)
  go ws0 0
  PM.unsafeFreezeByteArray marr

unpackByteArray :: ByteArray -> [Word8]
unpackByteArray arr = go 0 where
  go :: Int -> [Word8]
  go !ix = if ix < lengthByteArray arr
    then PM.indexByteArray arr ix : go (ix + 1)
    else []

lengthByteArray :: ByteArray -> Int
lengthByteArray = PM.sizeofByteArray
