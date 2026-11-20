{-# LANGUAGE DataKinds, ExperimentalDependentTypes, LinearTypes, StandaloneKindSignatures #-}
module PreludeFilterSource (FilterNegated, FilterWith) where

import GHC.Exts (Matchability(Unmatchable))
import qualified Prelude as P

-- | Keep the elements for which Boolean negation returns true.
type FilterNegated :: [P.Bool] -> [P.Bool]
type FilterNegated xs = P.filter P.not xs

-- | Filter a list whose predicate and spine can both be unknown.
type FilterWith :: (P.Bool %Unmatchable -> P.Bool) -> [P.Bool] -> [P.Bool]
type FilterWith p xs = P.filter p xs
