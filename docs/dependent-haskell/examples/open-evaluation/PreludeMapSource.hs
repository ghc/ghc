{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module PreludeMapSource (PreludeNegateAll, PreludeIdentityMap) where

import qualified Prelude as P

-- | Map Prelude's Boolean negation over a list whose spine may be unknown.
type PreludeNegateAll :: [P.Bool] -> [P.Bool]
type PreludeNegateAll xs = P.map P.not xs

-- | Map the ordinary polymorphic identity over a list with an unknown spine.
type PreludeIdentityMap :: forall a. [a] -> [a]
type PreludeIdentityMap xs = P.map P.id xs
