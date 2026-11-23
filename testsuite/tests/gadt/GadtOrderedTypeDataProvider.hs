{-# LANGUAGE DataKinds, GADTs, PolyKinds, TypeData, TypeOperators #-}
{-# LANGUAGE RequiredTypeArguments #-}
module GadtOrderedTypeDataProvider where

import Data.Kind (Type)

type data T a b where
  K :: forall a. a -> forall b. T a b

type data U a b where
  R :: forall a. a -> forall b -> Bool -> U a b

type data E a b where
  Q :: Bool -> forall a b. a ~ Int => E a b

type data H a where
  (:&) :: Bool -> forall a. Bool -> H a

infixr 5 :&

type data F a where
  F0 :: forall a. a ~ Int => forall b. F (a, b)
