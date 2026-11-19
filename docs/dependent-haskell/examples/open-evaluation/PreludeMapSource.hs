{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module PreludeMapSource (PreludeNegateAll) where

import qualified Prelude as P

-- | Map Prelude's Boolean negation over a list whose spine may be unknown.
type PreludeNegateAll :: [P.Bool] -> [P.Bool]
type PreludeNegateAll xs = P.map P.not xs
