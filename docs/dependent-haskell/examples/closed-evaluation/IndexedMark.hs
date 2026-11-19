{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}

module IndexedMark (Mark (..), sample, readMark) where

import Numeric.Natural (Natural)

-- | A value with fields only at index True.
data Mark a (b :: Bool) where
    Off :: Mark a 'False
    Count :: a -> Mark a 'True
    Pair :: a -> a -> Mark a 'True

-- | Return one of the constructors at index True.
sample :: Natural -> Mark Natural 'True
sample 0 = Count 4
sample n = Pair n (n + 1)

-- | Read the fields at index True in source order.
readMark :: Mark Natural 'True -> Natural
readMark (Count n) = n
readMark (Pair x y) = 10 * x + y
