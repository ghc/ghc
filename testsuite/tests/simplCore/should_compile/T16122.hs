{-# LANGUAGE TypeApplications #-}
-- Test that the Core for f isn't "worse" than g's.
-- The optimized Core for f used to involve dictionary-passing. See #16122.
module T16122 (f, g) where

import Data.Int (Int64)

f :: Double -> Int64
f = round

g :: Double -> Int64
g = fromIntegral @Int @Int64 . round
