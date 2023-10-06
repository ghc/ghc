{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Num.BigNat
import GHC.Exts

-- just to ensure that (future) rewrite rules don't mess with the test
{-# NOINLINE foo #-}
foo (# #) = bigNatZero# (# #)

main = do
  case bigNatIsPowerOf2# (foo (# #)) of
    (# _ | #) -> putStrLn "Zero isn't a power of two"
    (# | w #) -> putStrLn $ "Zero is 2^" ++ show (W# w)
