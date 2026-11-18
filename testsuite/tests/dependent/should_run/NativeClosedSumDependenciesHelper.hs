module NativeClosedSumDependenciesHelper (first, second) where

import Numeric.Natural (Natural)

{-# NOINLINE first #-}
first :: [Natural] -> Natural
first _ = 10

{-# NOINLINE second #-}
second :: [Natural] -> Natural
second _ = 11
