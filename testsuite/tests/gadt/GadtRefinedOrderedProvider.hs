{-# LANGUAGE GADTs, RequiredTypeArguments, TypeFamilies, UnliftedDatatypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module GadtRefinedOrderedProvider where

import Data.Kind (Type)
import Data.Proxy
import GHC.Exts (Levity, RuntimeRep(..), TYPE, UnliftedType)

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

data Box x y :: UnliftedType where
  LazyBox :: forall a. a -> forall b -> Proxy b -> Box a b
  StrictBox :: forall a. !a -> forall b -> Proxy b -> Box a b

data family UnliftedFamily tag b :: UnliftedType
data instance UnliftedFamily Int b where
  End :: Bool -> forall b -> UnliftedFamily Int b

type LevityBox :: forall (l :: Levity). Type -> Type -> TYPE (BoxedRep l)
data LevityBox a b where
  LevityBox :: forall (l :: Levity) a. a -> forall b -> LevityBox @l a b
