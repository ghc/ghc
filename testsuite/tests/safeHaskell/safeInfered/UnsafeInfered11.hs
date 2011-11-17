{-# LANGUAGE Safe #-}
-- | Basic test to see if no safe infer flag works
module UnsafeInfered11 where

import UnsafeInfered11_A

g :: Int
g = f

