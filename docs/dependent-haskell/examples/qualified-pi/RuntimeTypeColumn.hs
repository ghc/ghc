{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}

-- | Read a bounded element from a column with a runtime type witness.
module RuntimeTypeColumn (Column(..), lookupColumn) where

import Data.Kind (Type)
import ParameterizedIndexLibrary (indexGeneric, mkFinGeneric)
import Type.Reflection (TypeRep, eqTypeRep, (:~~:)(HRefl))

-- | Store the element type with the values.
data Column where
    Column :: TypeRep a -> [a] -> Column

-- | Return an element when the stored type and offset both match.
lookupColumn :: foreach (a :: Type) -> Column -> Int -> Maybe a
lookupColumn wanted (Column actual values) offset =
    case eqTypeRep wanted actual of
        Just HRefl -> fmap (indexGeneric values) (mkFinGeneric values offset)
        Nothing -> Nothing
