{-# LANGUAGE NoImplicitPrelude, PolyKinds, DataKinds, TypeFamilies,
             UndecidableSuperClasses, RankNTypes, TypeOperators,
             FlexibleContexts, TypeSynonymInstances, FlexibleInstances,
             UndecidableInstances #-}
module Monolith where

import Data.Kind (Type)
import GHC.Exts (Constraint)

type family (~>) :: c -> c -> Type

type instance (~>) = (->)
type instance (~>) = ArrPair

type family Fst (p :: (a, b)) :: a where
  Fst '(x, _) = x

type family Snd (p :: (a, b)) :: b where
  Snd '(_, y) = y

data ArrPair a b = ArrPair (Fst a ~> Fst b) (Snd a ~> Snd b)

type family Super c :: Constraint where
  Super Type = ()
  Super (c, d) = (Category c, Category d)

class Super cat => Category cat where
  id :: forall (a :: cat). a ~> a

instance Category Type where
  id = \x -> x

instance (Category c, Category d) => Category (c, d) where
  id = ArrPair id id

-- The commented out version worked
-- class Category (c, d) => Functor (f :: c -> d) where
class (Category c, Category d) => Functor (f :: c -> d) where
  map :: (a ~> b) -> (f a ~> f b)

data OnSnd f a b = OnSnd (f '(a, b))

instance Functor (f :: (c, d) -> Type) => Functor (OnSnd f a) where
  map f (OnSnd x) = OnSnd (map (ArrPair id f) x)
