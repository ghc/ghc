{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}

-- | Read a bounded element from a column with a runtime type witness.
module RuntimeTypeColumn
    (Column(..), ColumnChoice(..), lookupColumn, lookupColumnInferred, lookupColumnWhen) where

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

-- | Use the requested result type to choose the column's element type.
lookupColumnInferred :: foreach (a :: Type). Column -> Int -> Maybe a
lookupColumnInferred = lookupColumn a

-- | Request an offset only when the column is enabled.
data ColumnChoice (enabled :: Bool) where
    SkipColumn :: ColumnChoice False
    ReadColumn :: Int -> ColumnChoice True

-- | Read the selected column at the requested element type.
lookupColumnWhen :: foreach (enabled :: Bool) -> foreach (a :: Type).
                    ColumnChoice enabled -> Column -> Maybe a
lookupColumnWhen _ choice column =
    case choice of
        SkipColumn -> Nothing
        ReadColumn offset -> lookupColumn a column offset
