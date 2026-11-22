{-# LANGUAGE GADTs, RequiredTypeArguments, TypeFamilies #-}
module GadtRefinedOrderedProvider where

import Data.Proxy

data Refined x y where
  Same :: forall a. Eq a => !a -> forall b -> Proxy b -> Refined a a
  List :: forall a. Eq a => !a -> forall b -> Proxy b -> Refined [a] a
  Fixed :: forall a. Eq a => !a -> forall b -> Proxy b -> Refined Int Bool

data TrailingType x where
  TrailingType :: forall a. Eq a => !a -> forall b -> TrailingType [b]

data TrailingContext x y where
  TrailingContext :: forall a. Proxy a -> Eq a => TrailingContext a a

-- The wrapper casts to the family result after taking the written arguments.
data family Family tag x y
data instance Family Char x y where
  Family :: forall a. !a -> forall b -> Eq a => Proxy b -> Family Char [a] a
