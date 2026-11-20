{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds, StandaloneKindSignatures #-}
module PreludeFilterSource (FilterNegated, FilterWith, Negation) where

import GHC.Exts (U)
import qualified Prelude as P

-- | Keep the elements for which Boolean negation returns true.
type FilterNegated :: [P.Bool] -> [P.Bool]
type FilterNegated xs = P.filter P.not xs

-- | Filter a list whose predicate and spine can both be unknown.
type FilterWith :: forall a. (a %U -> P.Bool) -> [a] -> [a]
type FilterWith p xs = P.filter p xs

-- | Use installed Boolean negation as a predicate in a type.
type Negation :: P.Bool %U -> P.Bool
type Negation = P.not
