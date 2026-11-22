{-# LANGUAGE GADTs, RequiredTypeArguments #-}
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
