{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module CompareAt (Witness(..), decideCompare, keepCompare) where

import Data.Type.Equality ((:~:)(Refl))
import RuntimeOrdering (Witness(..))

-- | Observe the comparison at both runtime arguments.
decideCompare :: foreach (cmp :: Bool -> Bool -> Ordering)
              -> foreach (x :: Bool) -> foreach (y :: Bool) -> Witness (cmp x y)
decideCompare cmp x y = case cmp x y of
    LT -> Less
    EQ -> Equal
    GT -> Greater

-- | Return reflexivity without evaluating the call.
keepCompare :: foreach (cmp :: Bool -> Bool -> Ordering)
            -> foreach (x :: Bool) -> foreach (y :: Bool) -> cmp x y :~: cmp x y
keepCompare _ _ _ = Refl
