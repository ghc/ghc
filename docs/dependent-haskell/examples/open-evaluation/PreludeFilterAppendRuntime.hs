{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeFilterAppendRuntime (RuntimeFilterAppend(..), packFilterAppend, decisionsForNot) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:))
import PreludeAppendLengthSource (Appended)
import PreludeFilterAppendProof (FilterDecisions(..), filterAppend)
import PreludeFilterRuntime (negationResult)
import PreludeFilterSource (FilterWith, Negation)

-- | Store the input lists and the filter equality for those lists.
data RuntimeFilterAppend where
    RuntimeFilterAppend :: foreach (xs :: [Bool]) -> foreach (ys :: [Bool])
                        -> FilterWith Negation (Appended xs ys)
                           :~: Appended (FilterWith Negation xs) (FilterWith Negation ys)
                        -> RuntimeFilterAppend

-- | Package the filter equality for a finite left list of defined Booleans.
packFilterAppend :: [Bool] -> [Bool] -> RuntimeFilterAppend
packFilterAppend xs ys = RuntimeFilterAppend xs ys (filterAppendAt xs ys)

-- | Prove distribution by checking the actual negation result at each left head.
filterAppendAt :: foreach (xs :: [Bool]) -> foreach (ys :: [Bool])
               -> FilterWith Negation (Appended xs ys)
                  :~: Appended (FilterWith Negation xs) (FilterWith Negation ys)
filterAppendAt xs ys = filterAppend (decisionsForNot xs) (Proxy @ys)

-- | Record the negation decision at each head as the proof needs it.
decisionsForNot :: foreach (xs :: [Bool]) -> FilterDecisions Negation xs
decisionsForNot [] = Done
decisionsForNot (x : xs) = Decide (negationResult x) (decisionsForNot xs)
