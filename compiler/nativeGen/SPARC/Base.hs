
-- | Bits and pieces on the bottom of the module dependency tree.
--	Also import the required constants, so we know what we're using.
--	
--	In the interests of cross-compilation, we want to free ourselves
--	from the autoconf generated modules like main/Constants
--
module SPARC.Base (
	wordLength,
	wordLengthInBits,
	spillAreaLength,
	spillSlotSize,
	fits13Bits,
	largeOffsetError
)

where

import qualified Constants
import Panic

-- On 32 bit SPARC, pointers are 32 bits.
wordLength :: Int
wordLength = 4

wordLengthInBits :: Int
wordLengthInBits 
	= wordLength * 8

-- Size of the available spill area
spillAreaLength :: Int
spillAreaLength
	= Constants.rESERVED_C_STACK_BYTES

-- | We need 8 bytes because our largest registers are 64 bit.
spillSlotSize :: Int
spillSlotSize = 8


{-# SPECIALIZE fits13Bits :: Int -> Bool, Integer -> Bool #-}
-- | Check whether an offset is representable with 13 bits.
fits13Bits :: Integral a => a -> Bool
fits13Bits x = x >= -4096 && x < 4096


-- | Sadness.
largeOffsetError :: Integral a => a -> b
largeOffsetError i
  = panic ("ERROR: SPARC native-code generator cannot handle large offset ("
		++ show i ++ ");\nprobably because of large constant data structures;" ++ 
		"\nworkaround: use -fvia-C on this module.\n")
