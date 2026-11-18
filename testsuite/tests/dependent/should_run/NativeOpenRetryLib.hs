{-# LANGUAGE NoExperimentalDependentTypes #-}
module NativeOpenRetryLib (total) where

import Numeric.Natural (Natural)

total :: [Natural] -> Natural
total = sum
