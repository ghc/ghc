{-# LANGUAGE BangPatterns, CPP, MagicHash #-}

{- Note!
   If you see output like

        Expected: 48042
          Actual: 48042

   from this test, it means that the "actual" Word16 (or Word32)
   has higher bits that are nonzero, which is not allowed!
   See GHC.Word for an explanation.
-}

module Main ( main ) where

import Data.Bits
import GHC.Prim
import GHC.Word

#include "MachDeps.h"

main :: IO ()
main = do putStrLn test_primop_bSwap16
          putStrLn test_primop_bSwap32
          putStrLn test_primop_bSwap64
          putStrLn test_base_bSwap16
          putStrLn test_base_bSwap32
          putStrLn test_base_bSwap64
          putStrLn test'_base_bSwap16
          putStrLn test'_base_bSwap32
          putStrLn test'_base_bSwap64

bswap16 :: Word16 -> Word16
bswap16 (W16# w#) = W16# (narrow16Word# (byteSwap16# w#))

bswap32 :: Word32 -> Word32
bswap32 (W32# w#) = W32# (narrow32Word# (byteSwap32# w#))

bswap64 :: Word64 -> Word64
bswap64 (W64# w#) = W64# (byteSwap64# w#)

slowBswap64 :: Word64 -> Word64
slowBswap64 w =
        (w `shiftR` 56)                  .|. (w `shiftL` 56)
    .|. ((w `shiftR` 40) .&. 0xff00)     .|. ((w .&. 0xff00) `shiftL` 40)
    .|. ((w `shiftR` 24) .&. 0xff0000)   .|. ((w .&. 0xff0000) `shiftL` 24)
    .|. ((w `shiftR` 8)  .&. 0xff000000) .|. ((w .&. 0xff000000) `shiftL` 8)

-- | swap endianness on a Word32
slowBswap32 :: Word32 -> Word32
slowBswap32 w =
         (w `shiftR` 24)             .|. (w `shiftL` 24)
     .|. ((w `shiftR` 8) .&. 0xff00) .|. ((w .&. 0xff00) `shiftL` 8)

-- | swap endianness on a Word16
slowBswap16 :: Word16 -> Word16
slowBswap16 w = (w `shiftR` 8) .|. (w `shiftL` 8)

-- Test the primops directly.
test_primop_bSwap16 = test casesW16 bswap16 slowBswap16
test_primop_bSwap32 = test casesW32 bswap32 slowBswap32
test_primop_bSwap64 = test casesW64 bswap64 slowBswap64

-- Test the wrappers in GHC.Word, inlined.
-- Inlining matters because it means we are
-- testing the backend used to run the test,
-- rather than the backend used to build base.
test_base_bSwap16 = test casesW16 byteSwap16 slowBswap16
test_base_bSwap32 = test casesW32 byteSwap32 slowBswap32
test_base_bSwap64 = test casesW64 byteSwap64 slowBswap64

-- Test the wrappers in GHC.Word, not inlined.
test'_base_bSwap16 = test' casesW16 byteSwap16 slowBswap16
test'_base_bSwap32 = test' casesW32 byteSwap32 slowBswap32
test'_base_bSwap64 = test' casesW64 byteSwap64 slowBswap64

{-# INLINE test #-}
test :: (Eq a, Show a, Num a) => [a] -> (a -> a) -> (a -> a) -> String
test cases fast slow = case failing of
    [] -> "OK"
    ((_, e, a, i):xs) ->
        "FAIL\n" ++ "   Input: " ++ show i ++ "\nExpected: " ++ show e ++
        "\n  Actual: " ++ show a
  where
    failing = dropWhile ( \(b,_,_,_) -> b)
              . map (\ x -> (slow x == fast x, slow x, fast x, x)) $ cases
    expected = map slow cases
    actual = map fast cases

{-# NOINLINE test' #-}
test' :: (Eq a, Show a, Num a) => [a] -> (a -> a) -> (a -> a) -> String
test' = test

casesW16 = [0xff00,0xf021,0x1234,0x5620,0x5463,0x0000,0xa00f,0x0201,0x2901,
            0xaabb]
casesW32 = [1480294021,1626858410,2316287658,1246556957,3806579062,65945563,
            1521588071,791321966,1355466914,2284998160]
casesW64 = [11004539497957619752,5625461252166958202,1799960778872209546,
            16979826074020750638,12789915432197771481,11680809699809094550,
            13208678822802632247,13794454868797172383,13364728999716654549,
            17516539991479925226]
