{-# Language RankNTypes, ConstraintKinds, DataKinds, PolyKinds, GADTs #-}

module T14939 where

import Data.Kind

type Cat ob = ob -> ob -> Type

type Alg cls ob = ob

newtype Frí (cls::Type -> Constraint) :: (Type -> Alg cls Type) where
  Frí :: { with :: forall x. cls x => (a -> x) -> x }
      -> Frí cls a

data AlgCat (cls::Type -> Constraint) :: Cat (Alg cls Type) where
  AlgCat :: (cls a, cls b) => (a -> b) -> AlgCat cls a b

leftAdj :: AlgCat cls (Frí cls a) b -> (a -> b)
leftAdj (AlgCat f) a = undefined