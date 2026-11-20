{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module PreludeFilterRuntime
    ( RuntimeFilterBound(..), runtimeFilterBound, packRuntimeBound
    , showRuntimeBound ) where

import Prelude (Bool(..), String, show)
import qualified Prelude as P
import PreludeFilterLengthProof
    (AtMost, FilterSteps(End), filterLengthBound, keepIfTrue, skipIfFalse)
import PreludeFilterSource (FilterWith, Negation)
import PreludeMapLengthSize (size)
import PreludeMapLengthSource (Count)

-- | Retain the result of a Boolean test in either branch.
data BooleanResult (b :: Bool) where
    IsTrue :: BooleanResult 'True
    IsFalse :: BooleanResult 'False

-- | Evaluate the Boolean and return its constructor evidence.
booleanResult :: foreach (b :: Bool) -> BooleanResult b
booleanResult True = IsTrue
booleanResult False = IsFalse

-- | Derive one step from the predicate result at each head.
runtimeSteps :: foreach (xs :: [Bool]) -> FilterSteps Negation xs
runtimeSteps [] = End
runtimeSteps (x : xs) = case booleanResult (P.not x) of
    IsTrue -> keepIfTrue (runtimeSteps xs)
    IsFalse -> skipIfFalse (runtimeSteps xs)

-- | Bound the filtered size of a finite list with defined Boolean elements.
runtimeFilterBound :: foreach (xs :: [Bool]) -> AtMost (Count (FilterWith Negation xs)) (Count xs)
runtimeFilterBound xs = filterLengthBound (runtimeSteps xs)

-- | Store the input list with the bound that refers to that list.
data RuntimeFilterBound where
    RuntimeFilterBound :: foreach (xs :: [Bool])
                       -> AtMost (Count (FilterWith Negation xs)) (Count xs)
                       -> RuntimeFilterBound

-- | Package the list and its bound for an ordinary caller.
packRuntimeBound :: [Bool] -> RuntimeFilterBound
packRuntimeBound xs = RuntimeFilterBound xs (runtimeFilterBound xs)

-- | Show the bound and the sizes of the filtered and original lists.
showRuntimeBound :: RuntimeFilterBound -> String
showRuntimeBound (RuntimeFilterBound xs bound) =
    show (bound, size (P.filter P.not xs), size xs)
