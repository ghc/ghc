{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe       #-}
#endif

{-# OPTIONS_HADDOCK hide #-}

-- | Module      : Data.UUID.Types.Internal.Builder
-- Copyright   : (c) 2009 Mark Lentczner
--
-- License     : BSD-style
--
-- Maintainer  : markl@glyphic.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides a system that can call a function that takes
-- a sequence of some number of Word8 arguments.
--
-- The twist is that the Word8 arguments can be supplied directly
-- from Word8s, or from other sources that may provide more than
-- one Word8 apiece. Examples are Word16 and Word32 that supply
-- two and four Word8s respectively. Other ByteSource instances
-- can be defined.
--
-- This module is admittedly overkill. There are only three places
-- in the uuid package that need to call buildFromBytes with 16
-- Word8 values, but each place uses Words of different lengths:
--      version 1 uuids: 32-16-16-16-8-8-8-8-8-8
--      version 4 uuids: 24-24-32-24-24
--      version 5 uuids: 32-32-32-32
-- Originally, these three constructions were hand coded but the
-- code was ungainly. Using this module makes the code very
-- concise, and turns out to optimize to just as fast, or faster!

module Data.UUID.Types.Internal.Builder
    (ByteSource(..)
    ,ByteSink
    ,Takes1Byte
    ,Takes2Bytes
    ,Takes3Bytes
    ,Takes4Bytes
    ) where

import Data.Bits
import Data.Word



type Takes1Byte  g = Word8 -> g
type Takes2Bytes g = Word8 -> Word8 -> g
type Takes3Bytes g = Word8 -> Word8 -> Word8 -> g
type Takes4Bytes g = Word8 -> Word8 -> Word8 -> Word8 -> g
type Takes8Bytes g = Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> g

-- | Type of function that a given ByteSource needs.
-- This function must take as many Word8 arguments as the ByteSource provides
type family ByteSink w g
type instance ByteSink Word8  g = Takes1Byte g
type instance ByteSink Word16 g = Takes2Bytes g
type instance ByteSink Word32 g = Takes4Bytes g
type instance ByteSink Word64 g = Takes8Bytes g
type instance ByteSink Int    g = Takes4Bytes g


-- | Class of types that can add Word8s to a Builder.
-- Instances for Word8, Word16, Word32 and Int provide 1, 2, 4 and 4 bytes,
-- respectively, into a ByteSink
class ByteSource w where
    -- | Apply the source's bytes to the sink
    (/-/) :: ByteSink w g -> w -> g

infixl 6 /-/

instance ByteSource Word8 where
    f /-/ w = f w

instance ByteSource Word16 where
    f /-/ w = f b1 b2
        where b1 = fromIntegral (w `shiftR` 8)
              b2 = fromIntegral w

instance ByteSource Word32 where
    f /-/ w = f b1 b2 b3 b4
        where b1 = fromIntegral (w `shiftR` 24)
              b2 = fromIntegral (w `shiftR` 16)
              b3 = fromIntegral (w `shiftR` 8)
              b4 = fromIntegral w

instance ByteSource Word64 where
    f /-/ w = f b1 b2 b3 b4 b5 b6 b7 b8
        where b1 = fromIntegral (w `shiftR` 56)
              b2 = fromIntegral (w `shiftR` 48)
              b3 = fromIntegral (w `shiftR` 40)
              b4 = fromIntegral (w `shiftR` 32)
              b5 = fromIntegral (w `shiftR` 24)
              b6 = fromIntegral (w `shiftR` 16)
              b7 = fromIntegral (w `shiftR` 8)
              b8 = fromIntegral w

instance ByteSource Int where
    f /-/ w = f b1 b2 b3 b4
        where b1 = fromIntegral (w `shiftR` 24)
              b2 = fromIntegral (w `shiftR` 16)
              b3 = fromIntegral (w `shiftR` 8)
              b4 = fromIntegral w
