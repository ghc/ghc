{-# OPTIONS_GHC -fno-safe-infer #-}
-- | Basic test to see if no safe infer flag compiles
-- This module would usually infer safely, so it shouldn't be safe now.
-- We don't actually check that here though, see test '' for that.
module SafeFlags27 where

f :: Int
f = 1

