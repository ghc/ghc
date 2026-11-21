{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module RetainedFamilyTuple (readPair) where

import Data.Type.Equality ((:~:)(Refl))
import FamilyTuple (Tuple(Tup2))

-- | Return the pair's fields and a proof indexed by the retained argument.
readPair :: foreach (pair :: Tuple Bool Bool) -> (pair :~: pair, (Bool, Bool))
readPair pair = (Refl, case pair of Tup2 l r -> (l, r))
