{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1997-1998

\section[ByteTypes]{Deal with units of Words/Bytes}

This module defines a miscellaneously collection of very simple
types that

\begin{itemize}
\item have no other obvious home
\item don't depend on any other complicated types
\item are used in more than one "part" of the compiler
\end{itemize}

The types in this module in particular are used by both
GHCi and the NCG backend.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ByteTypes(
        -- * Words and bytes
        WordOff, ByteOff,
        wordsToBytes, bytesToWordsRoundUp,
        roundUpToWords, roundUpTo,

        StgWord, fromStgWord, toStgWord,
        StgHalfWord, fromStgHalfWord, toStgHalfWord,
        hALF_WORD_SIZE, hALF_WORD_SIZE_IN_BITS,
        wordSize
   ) where

import GhcPrelude

import BasicTypes( ConTagZ )
import DynFlags
import Outputable
import GHC.Platform
import FastString
import Binary

import Data.Word
import Data.Bits
import Data.ByteString (ByteString)

{-
************************************************************************
*                                                                      *
                Words and bytes
*                                                                      *
************************************************************************
-}

-- | Byte offset, or byte count
newtype ByteOff = ByteOff Int
    deriving (Enum, Eq, Integral, Num, Ord, Real)

-- | Word offset, or word count
newtype WordOff = WordOff Int
    deriving (Enum, Eq, Integral, Num, Ord, Real)

-- | Round up the given byte count to the next byte count that's a
-- multiple of the machine's word size.
roundUpToWords :: DynFlags -> ByteOff -> ByteOff
roundUpToWords dflags n = roundUpTo n (wORD_SIZE dflags)

-- | Round up @base@ to a multiple of @size@.
roundUpTo :: ByteOff -> ByteOff -> ByteOff
roundUpTo base size = (base + (size - 1)) .&. (complement (size - 1))

-- | Convert the given number of words to a number of bytes.
--
-- This function morally has type @WordOff -> ByteOff@, but uses @Num
-- a@ to allow for overloading.
wordsToBytes :: Num a => DynFlags -> a -> a
wordsToBytes dflags n = fromIntegral (wORD_SIZE dflags) * n
{-# SPECIALIZE wordsToBytes :: DynFlags -> Int -> Int #-}
{-# SPECIALIZE wordsToBytes :: DynFlags -> Word -> Word #-}
{-# SPECIALIZE wordsToBytes :: DynFlags -> Integer -> Integer #-}

-- | First round the given byte count up to a multiple of the
-- machine's word size and then convert the result to words.
bytesToWordsRoundUp :: DynFlags -> ByteOff -> WordOff
bytesToWordsRoundUp dflags n = (n + word_size - 1) `quot` word_size
 where word_size = wORD_SIZE dflags
-- StgWord is a type representing an StgWord on the target platform.
-- A Word64 is large enough to hold a Word for either a 32bit or 64bit platform
newtype StgWord = StgWord Word64
    deriving (Eq, Bits)

fromStgWord :: StgWord -> Integer
fromStgWord (StgWord i) = toInteger i

toStgWord :: DynFlags -> Integer -> StgWord
toStgWord dflags i
    = case platformWordSize (targetPlatform dflags) of
      -- These conversions mean that things like toStgWord (-1)
      -- do the right thing
      4 -> StgWord (fromIntegral (fromInteger i :: Word32))
      8 -> StgWord (fromInteger i :: Word64)
      w -> panic ("toStgWord: Unknown platformWordSize: " ++ show w)

instance Outputable StgWord where
    ppr (StgWord i) = integer (toInteger i)

--

-- A Word32 is large enough to hold half a Word for either a 32bit or
-- 64bit platform
newtype StgHalfWord = StgHalfWord Word32
    deriving Eq

fromStgHalfWord :: StgHalfWord -> Integer
fromStgHalfWord (StgHalfWord w) = toInteger w

toStgHalfWord :: DynFlags -> Integer -> StgHalfWord
toStgHalfWord dflags i
    = case platformWordSize (targetPlatform dflags) of
      -- These conversions mean that things like toStgHalfWord (-1)
      -- do the right thing
      4 -> StgHalfWord (fromIntegral (fromInteger i :: Word16))
      8 -> StgHalfWord (fromInteger i :: Word32)
      w -> panic ("toStgHalfWord: Unknown platformWordSize: " ++ show w)

instance Outputable StgHalfWord where
    ppr (StgHalfWord w) = integer (toInteger w)

hALF_WORD_SIZE :: DynFlags -> ByteOff
hALF_WORD_SIZE dflags = platformWordSize (targetPlatform dflags) `shiftR` 1
hALF_WORD_SIZE_IN_BITS :: DynFlags -> Int
hALF_WORD_SIZE_IN_BITS dflags = platformWordSize (targetPlatform dflags) `shiftL` 2

wordSize :: DynFlags -> ByteOff
wordSize dflags = ByteOff (wORD_SIZE dflags)