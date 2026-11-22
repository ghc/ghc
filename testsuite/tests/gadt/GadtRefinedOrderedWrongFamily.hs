{-# LANGUAGE GADTs, RequiredTypeArguments, TypeFamilies #-}
module GadtRefinedOrderedWrongFamily where

data family Family tag b
data instance Family Char b where
  Wrong :: Bool -> forall b -> Family Int b
