{-# LANGUAGE Haskell2010, NoExperimentalDependentTypes, TypeOperators #-}
module FamilyRefinementClient (refinedResult) where

import Data.Type.Equality ((:~:))
import FamilyTuple (C(Refines))
import RetainedFamilyTuple (RefinedInput, readRefinement)

-- | Check the refined index and read the runtime constructor.
refinedResult :: (RefinedInput :~: RefinedInput, Bool)
refinedResult = readRefinement Refines
