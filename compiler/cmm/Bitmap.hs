{-# LANGUAGE CPP #-}

--
-- (c) The University of Glasgow 2003-2006
--

-- Functions for constructing bitmaps, which are used in various
-- places in generated code (stack frame liveness masks, function
-- argument liveness masks, SRT bitmaps).

module Bitmap (
        Bitmap, mkBitmap,
        intsToBitmap, intsToReverseBitmap,
        mAX_SMALL_BITMAP_SIZE,
        seqBitmap,
  ) where

#include "HsVersions.h"
#include "../includes/MachDeps.h"

import SMRep
import DynFlags
import Util

import Data.Bits

{-|
A bitmap represented by a sequence of 'StgWord's on the /target/
architecture.  These are used for bitmaps in info tables and other
generated code which need to be emitted as sequences of StgWords.
-}
type Bitmap = [StgWord]

-- | Make a bitmap from a sequence of bits
mkBitmap :: DynFlags -> [Bool] -> Bitmap
mkBitmap _ [] = []
mkBitmap dflags stuff = chunkToBitmap dflags chunk : mkBitmap dflags rest
  where (chunk, rest) = splitAt (wORD_SIZE_IN_BITS dflags) stuff

chunkToBitmap :: DynFlags -> [Bool] -> StgWord
chunkToBitmap dflags chunk =
  foldr (.|.) (toStgWord dflags 0) [ toStgWord dflags 1 `shiftL` n | (True,n) <- zip chunk [0..] ]

-- | Make a bitmap where the slots specified are the /ones/ in the bitmap.
-- eg. @[0,1,3], size 4 ==> 0xb@.
--
-- The list of @Int@s /must/ be already sorted.
intsToBitmap :: DynFlags -> Int -> [Int] -> Bitmap
intsToBitmap dflags size slots{- must be sorted -}
  | size <= 0 = []
  | otherwise =
    (foldr (.|.) (toStgWord dflags 0) (map (toStgWord dflags 1 `shiftL`) these)) :
        intsToBitmap dflags (size - wORD_SIZE_IN_BITS dflags)
             (map (\x -> x - wORD_SIZE_IN_BITS dflags) rest)
   where (these,rest) = span (< wORD_SIZE_IN_BITS dflags) slots

-- | Make a bitmap where the slots specified are the /zeros/ in the bitmap.
-- eg. @[0,1,3], size 4 ==> 0x4@  (we leave any bits outside the size as zero,
-- just to make the bitmap easier to read).
--
-- The list of @Int@s /must/ be already sorted and duplicate-free.
intsToReverseBitmap :: DynFlags -> Int -> [Int] -> Bitmap
intsToReverseBitmap dflags size slots{- must be sorted -}
  | size <= 0 = []
  | otherwise =
    (foldr xor (toStgWord dflags init) (map (toStgWord dflags 1 `shiftL`) these)) :
        intsToReverseBitmap dflags (size - wORD_SIZE_IN_BITS dflags)
             (map (\x -> x - wORD_SIZE_IN_BITS dflags) rest)
   where (these,rest) = span (< wORD_SIZE_IN_BITS dflags) slots
         init
           | size >= wORD_SIZE_IN_BITS dflags = -1
           | otherwise                        = (1 `shiftL` size) - 1

{- |
Magic number, must agree with @BITMAP_BITS_SHIFT@ in InfoTables.h.
Some kinds of bitmap pack a size\/bitmap into a single word if
possible, or fall back to an external pointer when the bitmap is too
large.  This value represents the largest size of bitmap that can be
packed into a single word.
-}
mAX_SMALL_BITMAP_SIZE :: DynFlags -> Int
mAX_SMALL_BITMAP_SIZE dflags
 | wORD_SIZE dflags == 4 = 27
 | otherwise             = 58

seqBitmap :: Bitmap -> a -> a
seqBitmap = seqList

