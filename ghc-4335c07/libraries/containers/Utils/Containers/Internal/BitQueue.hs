{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Utils.Containers.Internal.BitQueue
-- Copyright   :  (c) David Feuer 2016
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- This contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.
--
-- = Description
--
-- An extremely light-weight, fast, and limited representation of a string of
-- up to (2*WORDSIZE - 2) bits. In fact, there are two representations,
-- misleadingly named bit queue builder and bit queue. The builder supports
-- only `emptyQB`, creating an empty builder, and `snocQB`, enqueueing a bit.
-- The bit queue builder is then turned into a bit queue using `buildQ`, after
-- which bits can be removed one by one using `unconsQ`. If the size limit is
-- exceeded, further operations will silently produce nonsense.
-----------------------------------------------------------------------------

module Utils.Containers.Internal.BitQueue
    ( BitQueue
    , BitQueueB
    , emptyQB
    , snocQB
    , buildQ
    , unconsQ
    , toListQ
    ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif
import Utils.Containers.Internal.BitUtil (shiftLL, shiftRL, wordSize)
import Data.Bits ((.|.), (.&.), testBit)
#if MIN_VERSION_base(4,8,0)
import Data.Bits (countTrailingZeros)
#elif MIN_VERSION_base(4,5,0)
import Data.Bits (popCount)
#endif

#if !MIN_VERSION_base(4,5,0)
-- We could almost certainly improve this fall-back (copied straight from the
-- default definition in Data.Bits), but it hardly seems worth the trouble
-- to speed things up on GHC 7.4 and below.
countTrailingZeros :: Word -> Int
countTrailingZeros x = go 0
      where
        go i | i >= wordSize      = i
             | testBit x i = i
             | otherwise   = go (i+1)

#elif !MIN_VERSION_base(4,8,0)
countTrailingZeros :: Word -> Int
countTrailingZeros x = popCount ((x .&. (-x)) - 1)
{-# INLINE countTrailingZeros #-}
#endif

-- A bit queue builder. We represent a double word using two words
-- because we don't currently have access to proper double words.
data BitQueueB = BQB {-# UNPACK #-} !Word
                     {-# UNPACK #-} !Word

newtype BitQueue = BQ BitQueueB deriving Show

-- Intended for debugging.
instance Show BitQueueB where
  show (BQB hi lo) = "BQ"++
    show (map (testBit hi) [(wordSize - 1),(wordSize - 2)..0]
            ++ map (testBit lo) [(wordSize - 1),(wordSize - 2)..0])

-- | Create an empty bit queue builder. This is represented as a single guard
-- bit in the most significant position.
emptyQB :: BitQueueB
emptyQB = BQB (1 `shiftLL` (wordSize - 1)) 0
{-# INLINE emptyQB #-}

-- Shift the double word to the right by one bit.
shiftQBR1 :: BitQueueB -> BitQueueB
shiftQBR1 (BQB hi lo) = BQB hi' lo' where
  lo' = (lo `shiftRL` 1) .|. (hi `shiftLL` (wordSize - 1))
  hi' = hi `shiftRL` 1
{-# INLINE shiftQBR1 #-}

-- | Enqueue a bit. This works by shifting the queue right one bit,
-- then setting the most significant bit as requested.
{-# INLINE snocQB #-}
snocQB :: BitQueueB -> Bool -> BitQueueB
snocQB bq b = case shiftQBR1 bq of
  BQB hi lo -> BQB (hi .|. (fromIntegral (fromEnum b) `shiftLL` (wordSize - 1))) lo

-- | Convert a bit queue builder to a bit queue. This shifts in a new
-- guard bit on the left, and shifts right until the old guard bit falls
-- off.
{-# INLINE buildQ #-}
buildQ :: BitQueueB -> BitQueue
buildQ (BQB hi 0) = BQ (BQB 0 lo') where
  zeros = countTrailingZeros hi
  lo' = ((hi `shiftRL` 1) .|. (1 `shiftLL` (wordSize - 1))) `shiftRL` zeros
buildQ (BQB hi lo) = BQ (BQB hi' lo') where
  zeros = countTrailingZeros lo
  lo1 = (lo `shiftRL` 1) .|. (hi `shiftLL` (wordSize - 1))
  hi1 = (hi `shiftRL` 1) .|. (1 `shiftLL` (wordSize - 1))
  lo' = (lo1 `shiftRL` zeros) .|. (hi1 `shiftLL` (wordSize - zeros))
  hi' = hi1 `shiftRL` zeros

-- Test if the queue is empty, which occurs when theres
-- nothing left but a guard bit in the least significant
-- place.
nullQ :: BitQueue -> Bool
nullQ (BQ (BQB 0 1)) = True
nullQ _ = False
{-# INLINE nullQ #-}

-- | Dequeue an element, or discover the queue is empty.
unconsQ :: BitQueue -> Maybe (Bool, BitQueue)
unconsQ q | nullQ q = Nothing
unconsQ (BQ bq@(BQB _ lo)) = Just (hd, BQ tl)
  where
    !hd = (lo .&. 1) /= 0
    !tl = shiftQBR1 bq
{-# INLINE unconsQ #-}

-- | Convert a bit queue to a list of bits by unconsing.
-- This is used to test that the queue functions properly.
toListQ :: BitQueue -> [Bool]
toListQ bq = case unconsQ bq of
      Nothing -> []
      Just (hd, tl) -> hd : toListQ tl
