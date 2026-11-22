{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies, TypeOperators #-}
module CompareAt (Witness(..), decideCompare, keepCompare, Result, decideFamilyCompare, keepFamilyCompare) where

import Data.Kind (Type)
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

-- | Select a comparison function or a Boolean result.
type family Result (tag :: Bool) :: Type where
    Result 'True = Bool -> Ordering
    Result 'False = Bool

-- | Observe the result of the function selected by the family.
decideFamilyCompare :: foreach (cmp :: Bool -> Result 'True)
                    -> foreach (x :: Bool) -> foreach (y :: Bool) -> Witness (cmp x y)
decideFamilyCompare cmp x y = case cmp x y of
    LT -> Less
    EQ -> Equal
    GT -> Greater

-- | Apply both arguments in the index without evaluating the comparison.
keepFamilyCompare :: foreach (cmp :: Bool -> Result 'True)
                  -> foreach (x :: Bool) -> foreach (y :: Bool) -> cmp x y :~: cmp x y
keepFamilyCompare _ _ _ = Refl
