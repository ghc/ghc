{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -fno-warn-noncanonical-monoid-instances
 -O2
#-}

module Packed.Bytes.Parser
  ( Parser(..)
  , Result(..)
  , Leftovers(..)
  , parseStreamST
  , any
  , failure
  ) where

import Control.Applicative
import Data.Primitive (ByteArray(..))
import GHC.Int (Int(I#))
import GHC.ST (ST(..),runST)
import GHC.Types (TYPE)
import GHC.Word (Word8(W8#))
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Stream.ST (ByteStream(..))
import Prelude hiding (any,replicate)

import qualified Data.Primitive as PM
import qualified Control.Monad

import GHC.Exts (Int#,ByteArray#,Word#,State#,(+#),(-#),(>#),indexWord8Array#)

type Bytes# = (# ByteArray#, Int#, Int# #)
type Maybe# (a :: TYPE r) = (# (# #) | a #)
type Leftovers# s = (# Bytes# , ByteStream s #)
type Result# s a = (# Maybe# (Leftovers# s), Maybe# a #)

data Result s a = Result
  { resultLeftovers :: !(Maybe (Leftovers s))
  , resultValue :: !(Maybe a)
  }

data Leftovers s = Leftovers
  { leftoversChunk :: {-# UNPACK #-} !Bytes
    -- ^ The last chunk pulled from the stream
  , leftoversStream :: ByteStream s
    -- ^ The remaining stream
  }

data PureResult a = PureResult
  { pureResultLeftovers :: {-# UNPACK #-} !Bytes
  , pureResultValue :: !(Maybe a)
  } deriving (Show)

emptyByteArray :: ByteArray
emptyByteArray = runST (PM.newByteArray 0 >>= PM.unsafeFreezeByteArray)

parseStreamST :: ByteStream s -> Parser a -> ST s (Result s a)
parseStreamST stream (Parser f) = ST $ \s0 ->
  case f (# | (# (# unboxByteArray emptyByteArray, 0#, 0# #), stream #) #) s0 of
    (# s1, r #) -> (# s1, boxResult r #)

boxResult :: Result# s a -> Result s a
boxResult (# leftovers, val #) = case val of
  (# (# #) | #) -> Result (boxLeftovers leftovers) Nothing
  (# | a #) -> Result (boxLeftovers leftovers) (Just a)

boxLeftovers :: Maybe# (Leftovers# s) -> Maybe (Leftovers s)
boxLeftovers (# (# #) | #) = Nothing
boxLeftovers (# | (# theBytes, stream #) #) = Just (Leftovers (boxBytes theBytes) stream)

instance Functor Parser where
  fmap = mapParser

-- Remember to write liftA2 by hand at some point.
instance Applicative Parser where
  pure = pureParser
  (<*>) = Control.Monad.ap

instance Monad Parser where
  return = pure
  (>>=) = bindLifted

newtype Parser a = Parser
  { getParser :: forall s.
       Maybe# (Leftovers# s)
    -> State# s
    -> (# State# s, Result# s a #)
  }

nextNonEmpty :: ByteStream s -> State# s -> (# State# s, Maybe# (Leftovers# s) #)
nextNonEmpty (ByteStream f) s0 = case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> (# s1, (# (# #) | #) #)
    (# | (# theBytes@(# _,_,len #), stream #) #) -> case len of
      0# -> nextNonEmpty stream s1
      _ -> (# s1, (# | (# theBytes, stream #) #) #)

withNonEmpty :: forall s b.
     Maybe# (Leftovers# s)
  -> State# s
  -> (State# s -> (# State# s, Result# s b #))
  -> (Word# -> Bytes# -> ByteStream s -> State# s -> (# State# s, Result# s b #))
     -- The first argument is a Word8, not a full machine word.
     -- The second argument is the complete,non-empty chunk
     -- with the head byte still intact.
  -> (# State# s, Result# s b #)
withNonEmpty (# (# #) | #) s0 g _ = g s0
withNonEmpty (# | (# bytes0@(# arr0,off0,len0 #), stream0 #) #) s0 g f = case len0 ># 0# of
  1# -> f (indexWord8Array# arr0 off0) bytes0 stream0 s0
  _ -> case nextNonEmpty stream0 s0 of
    (# s1, r #) -> case r of
      (# (# #) | #) -> g s1
      (# | (# bytes1@(# arr1, off1, _ #), stream1 #) #) ->
        f (indexWord8Array# arr1 off1) bytes1 stream1 s1

-- | Consume the next byte from the input.
any :: Parser Word8
any = Parser go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s Word8 #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# (# #) | #) #) #))
    (\theByte theBytes stream s ->
      (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | W8# theByte #) #) #)
    )

-- TODO: improve this
mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f p = bindLifted p (pureParser . f)

pureParser :: a -> Parser a
pureParser a = Parser $ \leftovers0 s0 ->
  (# s0, (# leftovers0, (# | a #) #) #)

bindLifted :: Parser a -> (a -> Parser b) -> Parser b
bindLifted (Parser f) g = Parser $ \leftovers0 s0 -> case f leftovers0 s0 of
  (# s1, (# leftovers1, val #) #) -> case val of
    (# (# #) | #) -> (# s1, (# leftovers1, (# (# #) | #) #) #)
    (# | x #) -> case g x of
      Parser k -> k leftovers1 s1

-- This assumes that the Bytes is longer than the index. It also does
-- not eliminate zero-length references to byte arrays.
unsafeDrop# :: Int# -> Bytes# -> Bytes#
unsafeDrop# i (# arr, off, len #) = (# arr, off +# i, len -# i #)

unboxByteArray :: ByteArray -> ByteArray#
unboxByteArray (ByteArray arr) = arr

boxBytes :: Bytes# -> Bytes
boxBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)

failure :: Parser a
failure = Parser (\m s -> (# s, (# m, (# (# #) | #) #) #))
