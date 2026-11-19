{-# LANGUAGE GADTs #-}
module NativeClosedWitnessFailures where

import ExistentialVec
import IndexedVec
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)

data Twin a where
  Twin :: Vec n a -> Vec n a -> Twin a

noWitness :: Natural -> Twin Natural
noWitness _ = errorWithoutStackTrace "ran a producer without a length witness"

-- Both vectors have the same representation. Supply the wrong length to
-- exercise the decoder's check against the independently observed witness.
misaligned :: Natural -> SomeVec Natural
misaligned n = SomeVec (SS SZ) (unsafeCoerce (VCons n (VCons (n + 1) VNil)))
