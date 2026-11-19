{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module ComputedPairProof (Selected, SelectedRight, proof, proofRight) where

import qualified ComputedPair as W
import Data.Type.Equality ((:~:) (Refl))
import qualified PairProjection as F

type Selected = F.pick (W.bump 7)

-- | The left projection does not evaluate the second field.
proof :: F.pick (W.bump 7) :~: 8
proof = Refl

type SelectedRight = F.pickRight (W.bumpRight 7)

-- | The right projection does not evaluate the first field.
proofRight :: F.pickRight (W.bumpRight 7) :~: 8
proofRight = Refl
