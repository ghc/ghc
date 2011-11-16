{-# LANGUAGE Safe #-}
-- | Basic test to see if no safe infer flag works
module UnsafeInfered10 where

import UnsafeInfered10_A

g :: Int
g = f

