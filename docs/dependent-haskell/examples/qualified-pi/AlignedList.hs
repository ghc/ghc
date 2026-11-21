{-# LANGUAGE ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeFamilies #-}

-- | Combine finite lists whose lengths agree.
module AlignedList (zipWithExact, checkedZipWith) where

import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Vec(..), sameNat, sourceLength)

-- | The result has the common length of the inputs.
zipWithExact :: forall a b c. foreach (xs :: [a]) -> foreach (ys :: [b])
             -> (sourceLength xs ~ sourceLength ys) => (a -> b -> c) -> Vec c (sourceLength xs)
zipWithExact [] [] _ = VNil
zipWithExact (x : xs) (y : ys) f = VCons (f x y) (zipWithExact xs ys f)

-- | Compare both spines before applying the callback to any element.
checkedZipWith :: forall a b c. foreach (xs :: [a]) -> foreach (ys :: [b])
               -> (a -> b -> c) -> Maybe (Vec c (sourceLength xs))
checkedZipWith xs ys f = case sameNat (sourceLength xs) (sourceLength ys) of
    Just Refl -> Just (zipWithExact xs ys f)
    Nothing -> Nothing
