{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module ConsRuleMapSource (NegateAll, NegateTwice) where

import qualified ConsRuleMap as M
import qualified Prelude as P

-- | Negate each Boolean while leaving an unknown tail abstract.
type NegateAll :: [P.Bool] -> [P.Bool]
type NegateAll xs = M.mapper P.not xs

-- | Negate each Boolean twice while leaving an unknown tail abstract.
type NegateTwice :: [P.Bool] -> [P.Bool]
type NegateTwice xs = M.mapper P.not (M.mapper P.not xs)
