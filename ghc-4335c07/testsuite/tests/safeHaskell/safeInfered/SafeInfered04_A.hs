{-# OPTIONS_GHC -fenable-rewrite-rules #-}
-- | Safe as while it enables rules it doesn't define any. UnsafeInfered05 is a
-- test case for when rules are defined and it should be unsafe
module SafeInfered04_A where

f :: Int
f = 1

