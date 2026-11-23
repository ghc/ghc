{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}

module SiblingColumnRow (packRow) where

import PackedVecLibrary (Vec)
import SiblingColumns (Columns, flags, letters, size)
import TypeLambda (Fields(..), Map)

-- | Put both columns in a row at their stored length.
packRow :: foreach (p :: Columns) -> Fields (Map (\a -> Vec a (size p)) '[Char, Bool])
packRow p = Field (letters p) (Field (flags p) End)
