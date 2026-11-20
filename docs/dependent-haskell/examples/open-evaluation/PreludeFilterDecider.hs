{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, TypeFamilies #-}
module PreludeFilterDecider
    ( FilterDecider(..), SomeFilterDecider(..), DecidedFilterBound(..)
    , Always, negationDecider, alwaysDecider, packDecidedBound, showDecidedBound ) where

import GHC.Exts (U)
import PreludeFilterLengthProof (AtMost)
import PreludeFilterRuntime (BooleanResult(..), negationResult, runtimeFilterBoundWith)
import PreludeFilterSource (FilterWith, Negation)
import PreludeMapLengthSize (size)
import PreludeMapLengthSource (Count)
import qualified Prelude as P
import Prelude (Bool(..), String, show)

-- | Supply a decision indexed by the result of the predicate at that input.
newtype FilterDecider (p :: Bool %U -> Bool) = FilterDecider
    (foreach (x :: Bool) -> BooleanResult (p x))

-- | Store a decider whose predicate is selected at runtime.
data SomeFilterDecider where
    SomeFilterDecider :: FilterDecider p -> SomeFilterDecider

-- | Store the input list and its filter size bound for the chosen predicate.
data DecidedFilterBound (p :: Bool %U -> Bool) where
    DecidedFilterBound :: foreach (xs :: [Bool])
                       -> AtMost (Count (FilterWith p xs)) (Count xs)
                       -> DecidedFilterBound p

-- | Package the bound for an ordinary caller.
packDecidedBound :: FilterDecider p -> [Bool] -> DecidedFilterBound p
packDecidedBound (FilterDecider decide) xs = DecidedFilterBound xs (runtimeFilterBoundWith decide xs)

-- | Recover the Boolean callback from the checked decision.
decisionBool :: FilterDecider p -> Bool -> Bool
decisionBool (FilterDecider decide) x = case decide x of
    IsTrue -> True
    IsFalse -> False

-- | Decide Boolean negation by calling Prelude.not.
negationDecider :: FilterDecider Negation
negationDecider = FilterDecider negationResult

-- | Keep every element regardless of its value.
type family Always (x :: Bool) :: Bool where
    Always _ = 'True

-- | Decide the constant-true predicate without forcing the element.
alwaysDecider :: FilterDecider Always
alwaysDecider = FilterDecider decide
  where
    decide :: foreach (x :: Bool) -> BooleanResult (Always x)
    decide _ = IsTrue

-- | Show the bound and compare it with the sizes before and after Prelude.filter.
showDecidedBound :: SomeFilterDecider -> [Bool] -> String
showDecidedBound (SomeFilterDecider decider) xs = case packDecidedBound decider xs of
    DecidedFilterBound ys bound -> show (bound, size (P.filter (decisionBool decider) ys), size ys)
