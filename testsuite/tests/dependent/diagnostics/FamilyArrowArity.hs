{-# LANGUAGE TypeFamilies #-}

module FamilyArrowArity where

import Data.Kind (Type)

type family Expected :: Type where
    Expected = Type

type Bad = (Maybe :: Expected)
