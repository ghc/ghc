{-# LANGUAGE RankNTypes, KindSignatures #-}
{-# OPTIONS_GHC -Werror #-}

-- #959

module ShouldFail where

import Data.Kind (Type)

data D (f :: (Type -> Type) -> Type -> Type)
       (af :: Type -> Type)
       (ax :: Type) =
  D (af (f af ax))

data CList (f :: (Type -> Type) -> Type -> Type) (a :: Type) =
  RCons a (CList (D f) a)

type CycleList a = forall f. CList f a

chead :: CycleList a -> a
chead ys = case ys of (RCons x xs) -> x
