{-# OPTIONS_GHC -fno-safe-infer #-}
-- | Basic test to see if no safe infer flag works
-- This module would usually infer safely, so it shouldn't be safe now.
module UnsafeInfered10_A where

f :: Int
f = 1

