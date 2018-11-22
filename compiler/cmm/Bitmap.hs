{-# LANGUAGE BangPatterns #-}

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

import GhcPrelude

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
  foldl' (.|.) (toStgWord dflags 0) [ oneAt n | (True,n) <- zip chunk [0..] ]
  where
    oneAt :: Int -> StgWord
    oneAt i = toStgWord dflags 1 `shiftL` i

-- | Make a bitmap where the slots specified are the /ones/ in the bitmap.
-- eg. @[0,1,3], size 4 ==> 0xb@.
--
-- The list of @Int@s /must/ be already sorted.
intsToBitmap :: DynFlags
             -> Int        -- ^ size in bits
             -> [Int]      -- ^ sorted indices of ones
             -> Bitmap
intsToBitmap dflags size = go 0
  where
    word_sz = wORD_SIZE_IN_BITS dflags
    oneAt :: Int -> StgWord
    oneAt i = toStgWord dflags 1 `shiftL` i

    -- It is important that we maintain strictness here.
    -- See Note [Strictness when building Bitmaps].
    go :: Int -> [Int] -> Bitmap
    go !pos slots
      | size <= pos = []
      | otherwise =
        (foldl' (.|.) (toStgWord dflags 0) (map (\i->oneAt (i - pos)) these)) :
          go (pos + word_sz) rest
      where
        (these,rest) = span (< (pos + word_sz)) slots

-- | Make a bitmap where the slots specified are the /zeros/ in the bitmap.
-- eg. @[0,1,3], size 4 ==> 0x4@  (we leave any bits outside the size as zero,
-- just to make the bitmap easier to read).
--
-- The list of @Int@s /must/ be already sorted and duplicate-free.
intsToReverseBitmap :: DynFlags
                    -> Int      -- ^ size in bits
                    -> [Int]    -- ^ sorted indices of zeros free of duplicates
                    -> Bitmap
intsToReverseBitmap dflags size = go 0
  where
    word_sz = wORD_SIZE_IN_BITS dflags
    oneAt :: Int -> StgWord
    oneAt i = toStgWord dflags 1 `shiftL` i

    -- It is important that we maintain strictness here.
    -- See Note [Strictness when building Bitmaps].
    go :: Int -> [Int] -> Bitmap
    go !pos slots
      | size <= pos = []
      | otherwise =
        (foldl' xor (toStgWord dflags init) (map (\i->oneAt (i - pos)) these)) :
          go (pos + word_sz) rest
      where
        (these,rest) = span (< (pos + word_sz)) slots
        remain = size - pos
        init
          | remain >= word_sz = -1
          | otherwise         = (1 `shiftL` remain) - 1

{-

Note [Strictness when building Bitmaps]
========================================

One of the places where @Bitmap@ is used is in in building Static Reference
Tables (SRTs) (in @CmmBuildInfoTables.procpointSRT@). In #7450 it was noticed
that some test cases (particularly those whose C-- have large numbers of CAFs)
produced large quantities of allocations from this function.

The source traced back to 'intsToBitmap', which was lazily subtracting the word
size from the elements of the tail of the @slots@ list and recursively invoking
itself with the result. This resulted in large numbers of subtraction thunks
being built up. Here we take care to avoid passing new thunks to the recursive
call. Instead we pass the unmodified tail along with an explicit position
accumulator, which get subtracted in the fold when we compute the Word.

-}

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

