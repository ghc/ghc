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
        (   test_pdep   ++ "\n"
        ++  test_pdep8  ++ "\n"
        ++  test_pdep16 ++ "\n"
        ++  test_pdep32 ++ "\n"
        ++  test_pdep64 ++ "\n"
        ++  "\n"
        )

class Pdep a where
  pdep :: a -> a -> a

instance Pdep Word where
  pdep (W#   src#) (W#   mask#) = W#   (pdep#   src# mask#)

instance Pdep Word8 where
  pdep (W8#  src#) (W8#  mask#) = W8#  (pdep8#  src# mask#)

instance Pdep Word16 where
  pdep (W16# src#) (W16# mask#) = W16# (pdep16# src# mask#)

instance Pdep Word32 where
  pdep (W32# src#) (W32# mask#) = W32# (pdep32# src# mask#)

instance Pdep Word64 where
  pdep (W64# src#) (W64# mask#) = W64# (pdep64# src# mask#)

class SlowPdep a where
  slowPdep :: a -> a -> a

instance SlowPdep Word where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Word8 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Word16 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Word32 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Word64 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

slowPdep64 :: Word64 -> Word64 -> Word64
slowPdep64 = slowPdep64' 0

slowPdep32 :: Word32 -> Word32 -> Word32
slowPdep32 s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

lsb :: Word64 -> Word64
lsb src = fromIntegral ((fromIntegral (src `shiftL` 63) :: Int64) `shiftR` 63)

slowPdep64' :: Word64 -> Word64 -> Word64 -> Word64
slowPdep64' result src mask = if lowest /= 0
  then slowPdep64' newResult (src `shiftR` 1) (mask .&. complement lowest)
  else result
  where lowest    = (-mask) .&. mask
        newResult = (result .|. ((lsb src) .&. lowest))

test_pdep   = test (0 :: Word  ) pdep slowPdep
test_pdep8  = test (0 :: Word8 ) pdep slowPdep
test_pdep16 = test (0 :: Word16) pdep slowPdep
test_pdep32 = test (0 :: Word32) pdep slowPdep
test_pdep64 = test (0 :: Word64) pdep slowPdep

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
