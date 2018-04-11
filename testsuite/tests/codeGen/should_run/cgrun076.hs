{-# LANGUAGE BangPatterns, CPP, MagicHash #-}

module Main ( main ) where

import Data.Bits
import GHC.Int
import GHC.Prim
import GHC.Word
import Data.Int
import Data.Word

#include "MachDeps.h"

main = putStr
        (   test_pext   ++ "\n"
        ++  test_pext8  ++ "\n"
        ++  test_pext16 ++ "\n"
        ++  test_pext32 ++ "\n"
        ++  test_pext64 ++ "\n"
        ++  "\n"
        )

class Pext a where
  pext :: a -> a -> a

instance Pext Word where
  pext (W#   src#) (W#   mask#) = W#   (pext#   src# mask#)

instance Pext Word8 where
  pext (W8#  src#) (W8#  mask#) = W8#  (pext8#  src# mask#)

instance Pext Word16 where
  pext (W16# src#) (W16# mask#) = W16# (pext16# src# mask#)

instance Pext Word32 where
  pext (W32# src#) (W32# mask#) = W32# (pext32# src# mask#)

instance Pext Word64 where
  pext (W64# src#) (W64# mask#) = W64# (pext64# src# mask#)

class SlowPext a where
  slowPext :: a -> a -> a

instance SlowPext Word where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word8 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word16 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word32 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word64 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

slowPext64 :: Word64 -> Word64 -> Word64
slowPext64 = slowPext64' 0 0 0

slowPext32 :: Word32 -> Word32 -> Word32
slowPext32 s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

slowPext64' :: Word64 -> Int -> Int -> Word64 -> Word64 -> Word64
slowPext64' result offset index src mask = if index /= 64
  then if maskBit /= 0
          then slowPext64' nextResult (offset + 1) (index + 1) src mask
          else slowPext64' result      offset      (index + 1) src mask
  else result
  where srcBit      = (src  `shiftR` index) .&. 1
        maskBit     = (mask `shiftR` index) .&. 1
        nextResult  = result .|. (srcBit `shiftL` offset)

test_pext   = test (0 :: Word  ) pext slowPext
test_pext8  = test (0 :: Word8 ) pext slowPext
test_pext16 = test (0 :: Word16) pext slowPext
test_pext32 = test (0 :: Word32) pext slowPext
test_pext64 = test (0 :: Word64) pext slowPext

mask n = (2 ^ n) - 1

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

runCase :: Eq a
        => (a -> a -> a)
        -> (a -> a -> a)
        -> (a, a)
        -> (Bool, a, a, (a, a))
runCase fast slow (x, y) = (slow x y == fast x y, slow x y, fast x y, (x, y))

test :: (Show a, Num a, Eq a) => a -> (a -> a -> a) -> (a -> a -> a) -> String
test _ fast slow = case failing of
    [] -> "OK"
    ((_, e, a, i):xs) ->
        "FAIL\n" ++ "   Input: " ++ show i ++ "\nExpected: " ++ show e ++
        "\n  Actual: " ++ show a
  where failing = dropWhile fst4 . map (runCase fast slow) $ cases
        cases   = (,) <$> numbers <*> numbers
        -- 10 random numbers
#if SIZEOF_HSWORD == 4
        numbers = [ 1480294021, 1626858410, 2316287658, 1246556957, 3806579062
                  , 65945563  , 1521588071, 791321966 , 1355466914, 2284998160
                  ]
#elif SIZEOF_HSWORD == 8
        numbers = [ 11004539497957619752, 5625461252166958202
                  , 1799960778872209546 , 16979826074020750638
                  , 12789915432197771481, 11680809699809094550
                  , 13208678822802632247, 13794454868797172383
                  , 13364728999716654549, 17516539991479925226
                  ]
#else
# error Unexpected word size
#endif
