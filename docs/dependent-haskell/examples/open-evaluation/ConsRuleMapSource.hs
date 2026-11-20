{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module ConsRuleMapSource (NegateAll) where

import qualified ConsRuleMap as M
import qualified Prelude as P

-- | Negate each Boolean while leaving an unknown tail abstract.
type NegateAll :: [P.Bool] -> [P.Bool]
type NegateAll xs = M.mapper P.not xs
