{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExplicitForAll, GADTs, KindSignatures, TypeFamilies #-}
module NativeClosedIndexFailures where

import IndexedVec
import Numeric.Natural (Natural)

empty :: Natural -> Vec 'Z Natural
empty _ = errorWithoutStackTrace "evaluated the producer for an ill-kinded call"

data Hidden where
  Pack :: forall (n :: N). Natural -> Hidden

hidden :: Natural -> Hidden
hidden _ = errorWithoutStackTrace "evaluated a producer with an unrecoverable index"

type family Collapse (n :: N) :: N where
  Collapse 'Z = 'Z
  Collapse ('S n) = 'Z

data Wrapped (i :: N) where
  Wrap :: forall (n :: N). Natural -> Wrapped (Collapse n)

blocked :: Natural -> Wrapped 'Z
blocked _ = errorWithoutStackTrace "evaluated a producer with a noninjective result index"

readHidden :: Hidden -> Natural
readHidden _ = errorWithoutStackTrace "evaluated an ambiguous erased index"
