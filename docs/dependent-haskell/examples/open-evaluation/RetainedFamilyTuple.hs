{-# LANGUAGE DataKinds, GADTs, ExperimentalDependentTypes, TypeOperators #-}
module RetainedFamilyTuple (readPair, RefinedInput, readRefinement) where

import Data.Type.Equality ((:~:)(Refl))
import FamilyTuple (Tuple(Tup2), C(Refines))

-- | Return the pair's fields and a proof indexed by the retained argument.
readPair :: foreach (pair :: Tuple Bool Bool) -> (pair :~: pair, (Bool, Bool))
readPair pair = (Refl, case pair of Tup2 l r -> (l, r))

-- | The promoted constructor at a concrete refined result kind.
type RefinedInput = ('Refines :: C (Int, Bool))

-- | Return a proof for the retained constructor and match its runtime value.
readRefinement :: foreach (t :: C (Int, Bool)) -> (t :~: t, Bool)
readRefinement t = (Refl, case t of Refines -> True)
