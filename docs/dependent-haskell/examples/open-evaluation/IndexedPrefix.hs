{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module IndexedPrefix (CheckedList, negatedCons, elements, withElements) where

import Data.Type.Equality ((:~:)(Refl))

-- | Store a runtime list together with its public index.
data CheckedList (xs :: [Bool]) where
    CheckedList :: foreach (ys :: [Bool]) -> (xs :~: ys) -> CheckedList xs

-- | Prepend a negated head at the same index used by the result type.
negatedCons :: foreach (b :: Bool) -> foreach (bs :: [Bool]) -> CheckedList (not b ': bs)
negatedCons b bs = CheckedList (not b : bs) Refl

-- | Read the stored list without inspecting its elements.
elements :: CheckedList xs -> [Bool]
elements (CheckedList ys _) = ys

-- | Pass the stored list and its index equality to a consumer.
withElements :: CheckedList xs
             -> (foreach (ys :: [Bool]) -> (xs :~: ys) -> r) -> r
withElements (CheckedList ys proof) k = k ys proof
