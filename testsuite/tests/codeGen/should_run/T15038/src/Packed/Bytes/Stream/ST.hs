{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

{-# OPTIONS_GHC -O2 #-}

module Packed.Bytes.Stream.ST
  ( ByteStream(..)
  , empty
  , unpack
  , fromBytes
  ) where

import Data.Primitive (Array,ByteArray(..))
import Data.Semigroup (Semigroup)
import Data.Word (Word8)
import GHC.Exts (RealWorld,State#,Int#,ByteArray#)
import GHC.Int (Int(I#))
import GHC.ST (ST(..))
import Packed.Bytes (Bytes(..))
import System.IO (Handle)
import qualified Data.Primitive as PM
import qualified Data.Semigroup as SG
import qualified Packed.Bytes as B

type Bytes# = (# ByteArray#, Int#, Int# #)

newtype ByteStream s = ByteStream
  (State# s -> (# State# s, (# (# #) | (# Bytes# , ByteStream s #) #) #) )

fromBytes :: Bytes -> ByteStream s
fromBytes b = ByteStream
  (\s0 -> (# s0, (# | (# unboxBytes b, empty #) #) #))

nextChunk :: ByteStream s -> ST s (Maybe (Bytes,ByteStream s))
nextChunk (ByteStream f) = ST $ \s0 -> case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> (# s1, Nothing #)
    (# | (# theBytes, theStream #) #) -> (# s1, Just (boxBytes theBytes, theStream) #)

empty :: ByteStream s
empty = ByteStream (\s -> (# s, (# (# #) | #) #) )

boxBytes :: Bytes# -> Bytes
boxBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)

unboxBytes :: Bytes -> Bytes#
unboxBytes (Bytes (ByteArray a) (I# b) (I# c)) = (# a,b,c #)

unpack :: ByteStream s -> ST s [Word8]
unpack stream = ST (unpackInternal stream)

unpackInternal :: ByteStream s -> State# s -> (# State# s, [Word8] #)
unpackInternal (ByteStream f) s0 = case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> (# s1, [] #)
    (# | (# bytes, stream #) #) -> case unpackInternal stream s1 of
      (# s2, ws #) -> (# s2, B.unpack (boxBytes bytes) ++ ws #)
