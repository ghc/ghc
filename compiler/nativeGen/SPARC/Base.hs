
-- | Bits and pieces on the bottom of the module dependency tree.
--      Also import the required constants, so we know what we're using.
--
--      In the interests of cross-compilation, we want to free ourselves
--      from the autoconf generated modules like main/Constants

module SPARC.Base (
        wordLength,
        wordLengthInBits,
        spillAreaLength,
        spillSlotSize,
        extraStackArgsHere,
        fits13Bits,
        is32BitInteger,
        largeOffsetError
)

where

import DynFlags
import Panic

import Data.Int


-- On 32 bit SPARC, pointers are 32 bits.
wordLength :: Int
wordLength = 4

wordLengthInBits :: Int
wordLengthInBits
        = wordLength * 8

-- Size of the available spill area
spillAreaLength :: DynFlags -> Int
spillAreaLength
        = rESERVED_C_STACK_BYTES

-- | We need 8 bytes because our largest registers are 64 bit.
spillSlotSize :: Int
spillSlotSize = 8


-- | We (allegedly) put the first six C-call arguments in registers;
--      where do we start putting the rest of them?
extraStackArgsHere :: Int
extraStackArgsHere = 23


{-# SPECIALIZE fits13Bits :: Int -> Bool, Integer -> Bool #-}
-- | Check whether an offset is representable with 13 bits.
fits13Bits :: Integral a => a -> Bool
fits13Bits x = x >= -4096 && x < 4096

-- | Check whether an integer will fit in 32 bits.
--      A CmmInt is intended to be truncated to the appropriate
--      number of bits, so here we truncate it to Int64.  This is
--      important because e.g. -1 as a CmmInt might be either
--      -1 or 18446744073709551615.
--
is32BitInteger :: Integer -> Bool
is32BitInteger i
        = i64 <= 0x7fffffff && i64 >= -0x80000000
        where i64 = fromIntegral i :: Int64


-- | Sadness.
largeOffsetError :: (Integral a, Show a) => a -> b
largeOffsetError i
  = panic ("ERROR: SPARC native-code generator cannot handle large offset ("
                ++ show i ++ ");\nprobably because of large constant data structures;" ++
                "\nworkaround: use -fvia-C on this module.\n")


