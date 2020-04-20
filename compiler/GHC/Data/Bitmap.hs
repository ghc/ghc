{-# LANGUAGE BangPatterns #-}

--
-- (c) The University of Glasgow 2003-2006
--

-- Functions for constructing bitmaps, which are used in various
-- places in generated code (stack frame liveness masks, function
-- argument liveness masks, SRT bitmaps).

module GHC.Data.Bitmap (
        Bitmap, mkBitmap,
        intsToReverseBitmap,
        mAX_SMALL_BITMAP_SIZE,
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Runtime.Heap.Layout

import Data.Bits

{-|
A bitmap represented by a sequence of 'StgWord's on the /target/
architecture.  These are used for bitmaps in info tables and other
generated code which need to be emitted as sequences of StgWords.
-}
type Bitmap = [StgWord]

-- | Make a bitmap from a sequence of bits
mkBitmap :: Platform -> [Bool] -> Bitmap
mkBitmap _ [] = []
mkBitmap platform stuff = chunkToBitmap platform chunk : mkBitmap platform rest
  where (chunk, rest) = splitAt (platformWordSizeInBits platform) stuff

chunkToBitmap :: Platform -> [Bool] -> StgWord
chunkToBitmap platform chunk =
  foldl' (.|.) (toStgWord platform 0) [ oneAt n | (True,n) <- zip chunk [0..] ]
  where
    oneAt :: Int -> StgWord
    oneAt i = toStgWord platform 1 `shiftL` i

-- | Make a bitmap where the slots specified are the /zeros/ in the bitmap.
-- eg. @[0,1,3], size 4 ==> 0x4@  (we leave any bits outside the size as zero,
-- just to make the bitmap easier to read).
--
-- The list of @Int@s /must/ be already sorted and duplicate-free.
intsToReverseBitmap :: Platform
                    -> Int      -- ^ size in bits
                    -> [Int]    -- ^ sorted indices of zeros free of duplicates
                    -> Bitmap
intsToReverseBitmap platform size = go 0
  where
    word_sz = platformWordSizeInBits platform
    oneAt :: Int -> StgWord
    oneAt i = toStgWord platform 1 `shiftL` i

    -- It is important that we maintain strictness here.
    -- See Note [Strictness when building Bitmaps].
    go :: Int -> [Int] -> Bitmap
    go !pos slots
      | size <= pos = []
      | otherwise =
        (foldl' xor (toStgWord platform init) (map (\i->oneAt (i - pos)) these)) :
          go (pos + word_sz) rest
      where
        (these,rest) = span (< (pos + word_sz)) slots
        remain = size - pos
        init
          | remain >= word_sz = -1
          | otherwise         = (1 `shiftL` remain) - 1

{-

Note [Strictness when building Bitmaps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One of the places where @Bitmap@ is used is in in building Static Reference
Tables (SRTs) (in @GHC.Cmm.Info.Build.procpointSRT@). In #7450 it was noticed
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
mAX_SMALL_BITMAP_SIZE :: Platform -> Int
mAX_SMALL_BITMAP_SIZE platform =
    case platformWordSize platform of
      PW4 -> 27 -- On 32-bit: 5 bits for size, 27 bits for bitmap
      PW8 -> 58 -- On 64-bit: 6 bits for size, 58 bits for bitmap
