{-# LANGUAGE MagicHash, ExtendedLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnboxedSums, UnboxedTuples #-}

-- needed on 32bit
{-# OPTIONS_GHC -fno-warn-overflowed-literals #-}

module Ex where

import GHC.Exts
import GHC.Word
import GHC.Int

-- Precise 'Int8#'/'Int8' range tests
exI8g1, exI8g2, exI8g3 :: Int8
exI8g1 = I8#  0x00#Int8
exI8g2 = I8#  0x7F#Int8
exI8g3 = I8# -0x80#Int8

-- Showcase various syntax for equivalent 'Int' terms
exIg1, exIg2, exIg3 :: Int
exIg1 =     0x7FFFFFFFFFFFFFFF
exIg2 = I#  0x7FFFFFFFFFFFFFFF#
exIg3 = I#  0x7FFFFFFFFFFFFFFF#Int

-- Motivating example: unboxed 'Word8#' parsing
data CEnum = Cons00 | Cons01 | ConsFF deriving Show
parseCEnum :: Word8# -> (# (##) | CEnum #)
parseCEnum = \case 0x00#Word8 -> (# | Cons00 #)
                   0x01#Word8 -> (# | Cons01 #)
                   0xFF#Word8 -> (# | ConsFF #)
                   _          -> (# (##) | #)

w8ToBool# :: Word8# -> Int#
w8ToBool# = \case 0#Word8 -> 0#
                  _       -> 1#

i8IsPole# :: Int8# -> Int#
i8IsPole# = \case 0x7F#Int8  -> 1#
                  -0x80#Int8 -> 1#
                  _          -> 0#
