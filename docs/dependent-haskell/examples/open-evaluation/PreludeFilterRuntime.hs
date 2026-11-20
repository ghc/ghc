{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module PreludeFilterRuntime
    ( BooleanResult(..), booleanResult, negationResult
    , RuntimeFilterBound(..), runtimeFilterBound, runtimeFilterBoundWith
    , runtimeFilterBoundFor, packRuntimeBound
    , showRuntimeBound ) where

import GHC.Exts (U)
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

-- | Retain the result of the installed Boolean negation.
negationResult :: foreach (x :: Bool) -> BooleanResult (Negation x)
negationResult x = booleanResult (P.not x)

-- | Ask the decider for one checked filter step at each head.
runtimeSteps :: forall a (p :: a %U -> Bool).
                (foreach (x :: a) -> BooleanResult (p x))
             -> foreach (xs :: [a]) -> FilterSteps p xs
runtimeSteps decide input = case input of
    [] -> End
    x : xs -> case decide x of
        IsTrue -> keepIfTrue (runtimeSteps @a @p decide xs)
        IsFalse -> skipIfFalse (runtimeSteps @a @p decide xs)

-- | Bound the filtered size of a finite list when its decider supplies each result.
runtimeFilterBoundFor :: forall a (p :: a %U -> Bool).
                         (foreach (x :: a) -> BooleanResult (p x))
                      -> foreach (xs :: [a])
                      -> AtMost (Count (FilterWith p xs)) (Count xs)
runtimeFilterBoundFor decide xs = filterLengthBound (runtimeSteps @a @p decide xs)

-- | Bound the filtered size of a finite Boolean list with a checked decider.
runtimeFilterBoundWith :: forall (p :: Bool %U -> Bool).
                          (foreach (x :: Bool) -> BooleanResult (p x))
                       -> foreach (xs :: [Bool])
                       -> AtMost (Count (FilterWith p xs)) (Count xs)
runtimeFilterBoundWith decide xs = runtimeFilterBoundFor @Bool @p decide xs

-- | Bound the filtered size of a finite list with defined Boolean elements.
runtimeFilterBound :: foreach (xs :: [Bool]) -> AtMost (Count (FilterWith Negation xs)) (Count xs)
runtimeFilterBound xs = runtimeFilterBoundWith @Negation negationResult xs

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
