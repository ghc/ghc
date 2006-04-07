--
-- (c) The University of Glasgow 2003
-- 

-- Functions for constructing bitmaps, which are used in various
-- places in generated code (stack frame liveness masks, function
-- argument liveness masks, SRT bitmaps).

module Bitmap ( 
	Bitmap, mkBitmap,
	intsToBitmap, intsToReverseBitmap,
	mAX_SMALL_BITMAP_SIZE
  ) where

#include "HsVersions.h"
#include "../includes/MachDeps.h"

import SMRep
import Constants
import DATA_BITS

{-|
A bitmap represented by a sequence of 'StgWord's on the /target/
architecture.  These are used for bitmaps in info tables and other
generated code which need to be emitted as sequences of StgWords.
-}
type Bitmap = [StgWord]

-- | Make a bitmap from a sequence of bits
mkBitmap :: [Bool] -> Bitmap
mkBitmap [] = []
mkBitmap stuff = chunkToBitmap chunk : mkBitmap rest
  where (chunk, rest) = splitAt wORD_SIZE_IN_BITS stuff

chunkToBitmap :: [Bool] -> StgWord
chunkToBitmap chunk = 
  foldr (.|.) 0 [ 1 `shiftL` n | (True,n) <- zip chunk [0..] ]

-- | Make a bitmap where the slots specified are the /ones/ in the bitmap.
-- eg. @[1,2,4], size 4 ==> 0xb@.
--
-- The list of @Int@s /must/ be already sorted.
intsToBitmap :: Int -> [Int] -> Bitmap
intsToBitmap size slots{- must be sorted -}
  | size <= 0 = []
  | otherwise = 
    (foldr (.|.) 0 (map (1 `shiftL`) these)) : 
	intsToBitmap (size - wORD_SIZE_IN_BITS) 
	     (map (\x -> x - wORD_SIZE_IN_BITS) rest)
   where (these,rest) = span (<wORD_SIZE_IN_BITS) slots

-- | Make a bitmap where the slots specified are the /zeros/ in the bitmap.
-- eg. @[1,2,4], size 4 ==> 0x8@  (we leave any bits outside the size as zero,
-- just to make the bitmap easier to read).
--
-- The list of @Int@s /must/ be already sorted.
intsToReverseBitmap :: Int -> [Int] -> Bitmap
intsToReverseBitmap size slots{- must be sorted -}
  | size <= 0 = []
  | otherwise = 
    (foldr xor init (map (1 `shiftL`) these)) : 
	intsToReverseBitmap (size - wORD_SIZE_IN_BITS) 
	     (map (\x -> x - wORD_SIZE_IN_BITS) rest)
   where (these,rest) = span (<wORD_SIZE_IN_BITS) slots
	 init
	   | size >= wORD_SIZE_IN_BITS = complement 0
	   | otherwise                 = (1 `shiftL` size) - 1

{- |
Magic number, must agree with @BITMAP_BITS_SHIFT@ in InfoTables.h.
Some kinds of bitmap pack a size\/bitmap into a single word if
possible, or fall back to an external pointer when the bitmap is too
large.  This value represents the largest size of bitmap that can be
packed into a single word.
-}
mAX_SMALL_BITMAP_SIZE :: Int
mAX_SMALL_BITMAP_SIZE  | wORD_SIZE == 4 = 27
		       | otherwise      = 58

