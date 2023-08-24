{-# LANGUAGE GADTs, RankNTypes, PolyKinds, TypeApplications,
             TypeAbstractions, ScopedTypeVariables, TypeFamilies #-}

module T18986a where

import Data.Kind (Type)

data T where
  MkT :: forall (f :: forall k. k -> Type).
    f Int -> f Maybe -> T

type family Id a where
  Id a = a

k1 :: T -> ()
k1 (MkT @(f :: forall k . k -> Type) (x :: f Int) (y :: f Maybe)) = ()

k2 :: T -> ()
k2 (MkT @f (x :: f Int) (y :: f Maybe)) = ()

k3 :: T -> ()
k3 (MkT @(f :: forall k . k -> Type) (x :: Id f (Id Int)) (y :: f Maybe)) = ()
