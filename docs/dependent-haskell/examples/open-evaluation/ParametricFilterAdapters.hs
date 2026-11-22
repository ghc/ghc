{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module ParametricFilterAdapters (boolToOrdering, isGT, isTrue) where

-- | Map each Boolean to an ordering. A bottom input gives a bottom result.
boolToOrdering :: Bool -> Ordering
boolToOrdering False = LT
boolToOrdering True = GT

-- | Select the greater-than result.
isGT :: Ordering -> Bool
isGT LT = False
isGT EQ = False
isGT GT = True

-- | The composition of 'isGT' and 'boolToOrdering'.
isTrue :: Bool -> Bool
isTrue False = False
isTrue True = True
