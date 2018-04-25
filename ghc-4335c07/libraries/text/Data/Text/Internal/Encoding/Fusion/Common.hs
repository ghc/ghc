{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Text.Internal.Encoding.Fusion.Common
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009,
--               (c) Duncan Coutts 2009,
--               (c) Jasper Van der Jeugt 2011
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Use at your own risk!
--
-- Fusible 'Stream'-oriented functions for converting between 'Text'
-- and several common encodings.

module Data.Text.Internal.Encoding.Fusion.Common
    (
    -- * Restreaming
    -- Restreaming is the act of converting from one 'Stream'
    -- representation to another.
      restreamUtf16LE
    , restreamUtf16BE
    , restreamUtf32LE
    , restreamUtf32BE
    ) where

import Data.Bits ((.&.))
import Data.Text.Internal.Fusion (Step(..), Stream(..))
import Data.Text.Internal.Fusion.Types (RS(..))
import Data.Text.Internal.Unsafe.Char (ord)
import Data.Text.Internal.Unsafe.Shift (shiftR)
import Data.Word (Word8)

restreamUtf16BE :: Stream Char -> Stream Word8
restreamUtf16BE (Stream next0 s0 len) = Stream next (RS0 s0) (len * 2)
  where
    next (RS0 s) = case next0 s of
        Done -> Done
        Skip s' -> Skip (RS0 s')
        Yield x s'
            | n < 0x10000 -> Yield (fromIntegral $ n `shiftR` 8) $
                             RS1 s' (fromIntegral n)
            | otherwise   -> Yield c1 $ RS3 s' c2 c3 c4
            where
              n  = ord x
              n1 = n - 0x10000
              c1 = fromIntegral (n1 `shiftR` 18 + 0xD8)
              c2 = fromIntegral (n1 `shiftR` 10)
              n2 = n1 .&. 0x3FF
              c3 = fromIntegral (n2 `shiftR` 8 + 0xDC)
              c4 = fromIntegral n2
    next (RS1 s x2)       = Yield x2 (RS0 s)
    next (RS2 s x2 x3)    = Yield x2 (RS1 s x3)
    next (RS3 s x2 x3 x4) = Yield x2 (RS2 s x3 x4)
    {-# INLINE next #-}
{-# INLINE restreamUtf16BE #-}

restreamUtf16LE :: Stream Char -> Stream Word8
restreamUtf16LE (Stream next0 s0 len) = Stream next (RS0 s0) (len * 2)
  where
    next (RS0 s) = case next0 s of
        Done -> Done
        Skip s' -> Skip (RS0 s')
        Yield x s'
            | n < 0x10000 -> Yield (fromIntegral n) $
                             RS1 s' (fromIntegral $ shiftR n 8)
            | otherwise   -> Yield c1 $ RS3 s' c2 c3 c4
          where
            n  = ord x
            n1 = n - 0x10000
            c2 = fromIntegral (shiftR n1 18 + 0xD8)
            c1 = fromIntegral (shiftR n1 10)
            n2 = n1 .&. 0x3FF
            c4 = fromIntegral (shiftR n2 8 + 0xDC)
            c3 = fromIntegral n2
    next (RS1 s x2)       = Yield x2 (RS0 s)
    next (RS2 s x2 x3)    = Yield x2 (RS1 s x3)
    next (RS3 s x2 x3 x4) = Yield x2 (RS2 s x3 x4)
    {-# INLINE next #-}
{-# INLINE restreamUtf16LE #-}

restreamUtf32BE :: Stream Char -> Stream Word8
restreamUtf32BE (Stream next0 s0 len) = Stream next (RS0 s0) (len * 2)
  where
    next (RS0 s) = case next0 s of
        Done       -> Done
        Skip s'    -> Skip (RS0 s')
        Yield x s' -> Yield c1 (RS3 s' c2 c3 c4)
          where
            n  = ord x
            c1 = fromIntegral $ shiftR n 24
            c2 = fromIntegral $ shiftR n 16
            c3 = fromIntegral $ shiftR n 8
            c4 = fromIntegral n
    next (RS1 s x2)       = Yield x2 (RS0 s)
    next (RS2 s x2 x3)    = Yield x2 (RS1 s x3)
    next (RS3 s x2 x3 x4) = Yield x2 (RS2 s x3 x4)
    {-# INLINE next #-}
{-# INLINE restreamUtf32BE #-}

restreamUtf32LE :: Stream Char -> Stream Word8
restreamUtf32LE (Stream next0 s0 len) = Stream next (RS0 s0) (len * 2)
  where
    next (RS0 s) = case next0 s of
        Done       -> Done
        Skip s'    -> Skip (RS0 s')
        Yield x s' -> Yield c1 (RS3 s' c2 c3 c4)
          where
            n  = ord x
            c4 = fromIntegral $ shiftR n 24
            c3 = fromIntegral $ shiftR n 16
            c2 = fromIntegral $ shiftR n 8
            c1 = fromIntegral n
    next (RS1 s x2)       = Yield x2 (RS0 s)
    next (RS2 s x2 x3)    = Yield x2 (RS1 s x3)
    next (RS3 s x2 x3 x4) = Yield x2 (RS2 s x3 x4)
    {-# INLINE next #-}
{-# INLINE restreamUtf32LE #-}
