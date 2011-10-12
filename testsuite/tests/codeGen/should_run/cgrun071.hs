{-# LANGUAGE BangPatterns, CPP, MagicHash #-}

module Main ( main ) where

import Data.Bits
import GHC.Prim
import GHC.Word

#include "MachDeps.h"

main = putStr
       (test_popCnt ++ "\n"
        ++ test_popCnt8 ++ "\n"
        ++ test_popCnt16 ++ "\n"
        ++ test_popCnt32 ++ "\n"
        ++ test_popCnt64 ++ "\n"
        ++ "\n"
       )

popcnt :: Word -> Word
popcnt (W# w#) = W# (popCnt# w#)

popcnt8 :: Word -> Word
popcnt8 (W# w#) = W# (popCnt8# w#)

popcnt16 :: Word -> Word
popcnt16 (W# w#) = W# (popCnt16# w#)

popcnt32 :: Word -> Word
popcnt32 (W# w#) = W# (popCnt32# w#)

popcnt64 :: Word64 -> Word
popcnt64 (W64# w#) =
#if SIZEOF_HSWORD == 4
    W# (popCnt64# w#)
#else
    W# (popCnt# w#)
#endif

-- Cribbed from http://hackage.haskell.org/trac/ghc/ticket/3563
slowPopcnt :: Word -> Word
slowPopcnt x = count' (bitSize x) x 0
  where
    count' 0 _ !acc = acc
    count' n x acc  = count' (n-1) (x `shiftR` 1)
                      (acc + if x .&. 1 == 1 then 1 else 0)

test_popCnt = test popcnt slowPopcnt
test_popCnt8 = test popcnt8 (slowPopcnt . fromIntegral . (mask 8 .&.))
test_popCnt16 = test popcnt16 (slowPopcnt . fromIntegral . (mask 16 .&.))
test_popCnt32 = test popcnt32 (slowPopcnt . fromIntegral . (mask 32 .&.))
test_popCnt64 = test popcnt64 (slowPopcnt . fromIntegral . (mask 64 .&.))

mask n = (2 ^ n) - 1

test :: (Show a, Num a) => (a -> Word) -> (a -> Word) -> String
test fast slow = case failing of
    [] -> "OK"
    ((_, e, a, i):xs) ->
        "FAIL\n" ++ "   Input: " ++ show i ++ "\nExpected: " ++ show e ++
        "\n  Actual: " ++ show a
  where
    failing = dropWhile ( \(b,_,_,_) -> b)
              . map (\ x -> (slow x == fast x, slow x, fast x, x)) $ cases
    expected = map slow cases
    actual = map fast cases
    -- 10 random numbers
#if SIZEOF_HSWORD == 4
    cases = [1480294021,1626858410,2316287658,1246556957,3806579062,65945563,
             1521588071,791321966,1355466914,2284998160]
#elif SIZEOF_HSWORD == 8
    cases = [11004539497957619752,5625461252166958202,1799960778872209546,
             16979826074020750638,12789915432197771481,11680809699809094550,
             13208678822802632247,13794454868797172383,13364728999716654549,
             17516539991479925226]
#else
# error Unexpected word size
#endif
