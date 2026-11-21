{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module NativeClosedWitnessFailures where

import ExistentialVec
import Data.Proxy (Proxy)
import IndexedVec
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)

data HiddenWidth where
  HiddenWidth :: Char -> Proxy (n :: N) -> HiddenWidth

noWitness :: Natural -> HiddenWidth
noWitness _ = errorWithoutStackTrace "ran a producer without a length witness"

-- Both vectors have the same representation. Supply the wrong length to
-- exercise the decoder's check against the independently observed witness.
misaligned :: Natural -> SomeVec Natural
misaligned n = SomeVec (SS SZ) (unsafeCoerce (VCons n (VCons (n + 1) VNil)))
