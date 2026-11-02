module Main where

import Data.Bits

-- Drive the long-division loop in bignat_quotrem_normalized: the divisor
-- spans several words, so quotRem takes the general Knuth Algorithm D
-- path rather than the single-word fast path of bignat_quotrem_word, and
-- the size gap between dividend and divisor gives ~m = (sizeA - sizeB)
-- outer iterations per call. Each of those iterations used to allocate a
-- fresh BigNat for qj*V, plus one more per quotient-digit correction
-- step; they now run as a fused in-place pass.
-- See Note [Knuth division loop] in ghc-internal:GHC.Internal.Bignum.Backend.Native.
--
-- Only meaningful with the native bignum backend: with the GMP backend
-- the division is done by GMP, so the test is skipped there.
--
-- The dividend is xor-ed with the loop counter so that each iteration
-- divides a distinct (but equally large) value: this keeps the division
-- from being floated out of the loop and computed just once.

loop :: Int -> Integer -> Integer -> Integer -> Integer
loop 0 acc _    _ = acc
loop n acc bigA b =
  let a      = bigA `xor` fromIntegral n
      (q, r) = a `quotRem` b
  in loop (n - 1) (acc + q + r) bigA b

main :: IO ()
main = do
  let bigA = 2 ^ (8192 :: Int) - 1 :: Integer
      b    = 2 ^ (2048 :: Int) + 2 ^ (1900 :: Int) + 1 :: Integer
      r    = loop 10000 0 bigA b
  -- Print only a small digest so the .stdout doesn't have to track a
  -- 5000-digit number, while still forcing the full computation.
  print (r `mod` 1000000007)
