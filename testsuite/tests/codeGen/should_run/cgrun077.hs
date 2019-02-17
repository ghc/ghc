{-# LANGUAGE BangPatterns, CPP, MagicHash #-}

module Main ( main ) where

import Data.Bits
import GHC.Prim
import GHC.Word

#include "MachDeps.h"

main = putStr
       (test_lzCnt ++ "\n"
        ++ test_lzCnt8 ++ "\n"
        ++ test_lzCnt16 ++ "\n"
        ++ test_lzCnt32 ++ "\n"
        ++ test_lzCnt64 ++ "\n"
        ++ "\n"
        ++ test_tzCnt ++ "\n"
        ++ test_tzCnt8 ++ "\n"
        ++ test_tzCnt16 ++ "\n"
        ++ test_tzCnt32 ++ "\n"
        ++ test_tzCnt64 ++ "\n"
        ++ "\n"
       )

lzcnt :: Word -> Word
lzcnt (W# w#) = W# (clz# w#)

lzcnt8 :: Word -> Word
lzcnt8 (W# w#) = W# (clz8# w#)

lzcnt16 :: Word -> Word
lzcnt16 (W# w#) = W# (clz16# w#)

lzcnt32 :: Word -> Word
lzcnt32 (W# w#) = W# (clz32# w#)

lzcnt64 :: Word64 -> Word
lzcnt64 (W64# w#) =
#if SIZEOF_HSWORD == 4
    W# (clz64# w#)
#else
    W# (clz# w#)
#endif

lzcnt_slow :: Int -> Word -> Word
lzcnt_slow size x = fromIntegral $ min size $ length $ takeWhile (== False) $ reverse $ map (testBit x) [0 .. size - 1]

tzcnt :: Word -> Word
tzcnt (W# w#) = W# (ctz# w#)

tzcnt8 :: Word -> Word
tzcnt8 (W# w#) = W# (ctz8# w#)

tzcnt16 :: Word -> Word
tzcnt16 (W# w#) = W# (ctz16# w#)

tzcnt32 :: Word -> Word
tzcnt32 (W# w#) = W# (ctz32# w#)

tzcnt64 :: Word64 -> Word
tzcnt64 (W64# w#) =
#if SIZEOF_HSWORD == 4
    W# (ctz64# w#)
#else
    W# (ctz# w#)
#endif

tzcnt_slow :: Int -> Word -> Word
tzcnt_slow size x = fromIntegral $ min size $ length $ takeWhile (== False) $ map (testBit x) [0 .. size - 1]

test_lzCnt = test "lzcnt" lzcnt (lzcnt_slow (8 * SIZEOF_HSWORD))
test_lzCnt8 = test "lzcnt8" lzcnt8 (lzcnt_slow 8 . fromIntegral . (mask 8 .&.))
test_lzCnt16 = test "lzcnt16" lzcnt16 (lzcnt_slow 16 . fromIntegral . (mask 16 .&.))
test_lzCnt32 = test "lzcnt32" lzcnt32 (lzcnt_slow 32 . fromIntegral . (mask 32 .&.))
test_lzCnt64 = test "lzcnt64" lzcnt64 (lzcnt_slow 64 . fromIntegral . (mask 64 .&.))

test_tzCnt = test "tzcnt" tzcnt (tzcnt_slow (8 * SIZEOF_HSWORD))
test_tzCnt8 = test "tzcnt8" tzcnt8 (tzcnt_slow 8 . fromIntegral . (mask 8 .&.))
test_tzCnt16 = test "tzcnt16" tzcnt16 (tzcnt_slow 16 . fromIntegral . (mask 16 .&.))
test_tzCnt32 = test "tzcnt32" tzcnt32 (tzcnt_slow 32 . fromIntegral . (mask 32 .&.))
test_tzCnt64 = test "tzcnt64" tzcnt64 (tzcnt_slow 64 . fromIntegral . (mask 64 .&.))

mask n = (2 ^ n) - 1

test :: (Show a, Num a) => String -> (a -> Word) -> (a -> Word) -> String
test name fast slow = case failing of
    [] -> "OK"
    ((_, e, a, i):xs) ->
        "FAIL " ++ name ++ "\n" ++ "   Input: " ++ show i ++ "\nExpected: " ++ show e ++
        "\n  Actual: " ++ show a
  where
    failing = dropWhile ( \(b,_,_,_) -> b)
              . map (\ x -> (slow x == fast x, slow x, fast x, x)) $ cases
    expected = map slow cases
    actual = map fast cases
#if SIZEOF_HSWORD == 4
    cases = [0, 1, 2, 1480294021,1626858410,2316287658,1246556957,3806579062,65945563,
             1521588071,791321966,1355466914,2284998160]
#elif SIZEOF_HSWORD == 8
    cases = [0, 1, 2, 11004539497957619752,5625461252166958202,1799960778872209546,
             16979826074020750638,12789915432197771481,11680809699809094550,
             13208678822802632247,13794454868797172383,13364728999716654549,
             17516539991479925226]
#else
# error Unexpected word size
#endif
