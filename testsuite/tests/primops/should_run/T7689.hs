{-# LANGUAGE BangPatterns, MagicHash #-}
module Main where

import Data.Bits (finiteBitSize)
import GHC.Exts

main :: IO ()
main = do
  -- 0 is the annihilator of andI#
  print (I# (maxI# `andI#`    0#) == 0)
  print (I# (minI# `andI#`    0#) == 0)
  print (I# (0#    `andI#` maxI#) == 0)
  print (I# (0#    `andI#` minI#) == 0)
  print (I# (0#    `andI#`    0#) == 0)
  -- integer with all bits set to 1 is the neutral element of orI#,
  -- in two's complement this is -1
  print (I# (maxI# `andI#`   -1#) == maxI)
  print (I# (minI# `andI#`   -1#) == minI)
  print (I# (-1#   `andI#` maxI#) == maxI)
  print (I# (-1#   `andI#` minI#) == minI)
  print (I# (-1#   `andI#`   -1#) == -1)
  -- these two numbers have every other bit set, they should give 0
  print (I# (magicInt1# `andI#` magicInt2#) == 0)

  -- integer with all bits set to 1 is the annihilator of orI#,
  print (I# (maxI# `orI#`    -1#) == -1)
  print (I# (minI# `orI#`    -1#) == -1)
  print (I# (-1#   `orI#`  maxI#) == -1)
  print (I# (-1#   `orI#`  minI#) == -1)
  print (I# (-1#   `orI#`    -1#) == -1)
  -- 0 is the neutral element of orI#
  print (I# (maxI# `orI#`     0#) == maxI)
  print (I# (minI# `orI#`     0#) == minI)
  print (I# (0#    `orI#`  maxI#) == maxI)
  print (I# (0#    `orI#`  minI#) == minI)
  print (I# (0#    `orI#`     0#) == 0)
  -- this time we should get an integer with all bits set, that is -1
  print (I# (magicInt1# `orI#` magicInt2#) == -1)

  -- suprising as the first two tests may look, this is what we expect from
  -- bitwise negation in two's complement enccoding
  print (I# (notI#  0#) == -1)
  print (I# (notI# -1#) ==  0)
  -- magic int numbers are bitwise complementary
  print (I# (notI# magicInt1#) == magicInt2)
  print (I# (notI# magicInt2#) == magicInt1)

  -- 0 is the identity of xor
  print (I# (minI# `xorI#`    0#) == minI)
  print (I# (maxI# `xorI#`    0#) == maxI)
  print (I# (0#    `xorI#` minI#) == minI)
  print (I# (0#    `xorI#` maxI#) == maxI)
  -- anything xored with itself is 0
  print (I# (maxI# `xorI#` maxI#) == 0)
  print (I# (minI# `xorI#` minI#) == 0)
  -- xoring with -1 is like bitwise negation (becuse -1 has all bits set to 1)
  print (I# (minI# `xorI#`   -1#) == maxI)
  print (I# (maxI# `xorI#`   -1#) == minI)
  print (I# (-1#   `xorI#` minI#) == maxI)
  print (I# (-1#   `xorI#` maxI#) == minI)
  -- since these two have exactly the opposite bits turned on they should
  -- give an int with all bits set, and that is -1 as you probably already
  -- remember by now
  print (I# (magicInt1# `xorI#` magicInt2#) == -1)
    where
      intBitSize = finiteBitSize (undefined :: Int)
      minI  = minBound :: Int
      maxI  = maxBound :: Int
      minI# = x
          where !(I# x) = minBound
      maxI# = x
          where !(I# x) = maxBound
      magicInt1 = sum $ map (2^) [0,2..intBitSize] :: Int
      magicInt2 = sum $ map (2^) [1,3..intBitSize] :: Int
      magicInt1# = x
          where !(I# x) = magicInt1
      magicInt2# = x
          where !(I# x) = magicInt2
