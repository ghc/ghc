{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}

module ExistentialVec (SN (..), SomeVec (..), someVec, readSomeVec) where

import IndexedVec (N (..), Vec (..))
import Numeric.Natural (Natural)

-- | A runtime witness for a vector length.
data SN (n :: N) where
    SZ :: SN 'Z
    SS :: SN n -> SN ('S n)

-- | A vector and its length witness.
data SomeVec a where
    SomeVec :: SN n -> Vec n a -> SomeVec a

-- | Build no cells at zero, one at an odd input, and two at a positive even input.
someVec :: Natural -> SomeVec Natural
someVec 0 = SomeVec SZ VNil
someVec n
    | odd n = SomeVec (SS SZ) (VCons n VNil)
    | otherwise = SomeVec (SS (SS SZ)) (VCons n (VCons (n + 1) VNil))

-- | Read the cells in source order.
readSomeVec :: SomeVec Natural -> [Natural]
readSomeVec (SomeVec _ xs) = toList xs
  where
    toList :: Vec n Natural -> [Natural]
    toList VNil = []
    toList (VCons x rest) = x : toList rest
