{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language NoImplicitPrelude #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language DefaultSignatures #-}
{-# language FunctionalDependencies #-}
{-# language UndecidableSuperClasses #-}
{-# language UndecidableInstances #-}
{-# language TypeInType #-}

module T11523 where

import GHC.Types (Constraint, Type)
import qualified Prelude

type Cat i = i -> i -> Type

newtype Y (p :: i -> j -> Type) (a :: j) (b :: i) = Y { getY :: p b a }

class Vacuous (a :: i)
instance Vacuous a

class (Functor p, Dom p ~ Op p, Cod p ~ Nat p (->)) => Category (p :: Cat i) where
  type Op p :: Cat i
  type Op p = Y p
  type Ob p :: i -> Constraint
  type Ob p = Vacuous

class (Category (Dom f), Category (Cod f)) => Functor (f :: i -> j) where
  type Dom f :: Cat i
  type Cod f :: Cat j

class    (Functor f, Dom f ~ p, Cod f ~ q) => Fun (p :: Cat i) (q :: Cat j) (f :: i -> j) | f -> p q
instance (Functor f, Dom f ~ p, Cod f ~ q) => Fun (p :: Cat i) (q :: Cat j) (f :: i -> j)

data Nat (p :: Cat i) (q :: Cat j) (f :: i -> j) (g :: i -> j)

instance (Category p, Category q) => Category (Nat p q) where
  type Ob (Nat p q) = Fun p q

instance (Category p, Category q) => Functor (Nat p q) where
  type Dom (Nat p q) = Y (Nat p q)
  type Cod (Nat p q) = Nat (Nat p q) (->)

instance (Category p, Category q) => Functor (Nat p q f) where
  type Dom (Nat p q f) = Nat p q
  type Cod (Nat p q f) = (->)

instance Category (->)

instance Functor ((->) e) where
  type Dom ((->) e) = (->)
  type Cod ((->) e) = (->)

instance Functor (->) where
  type Dom (->) = Y (->)
  type Cod (->) = Nat (->) (->)

instance (Category p, Op p ~ Y p) => Category (Y p) where
  type Op (Y p) = p

instance (Category p, Op p ~ Y p) => Functor (Y p a) where
  type Dom (Y p a) = Y p
  type Cod (Y p a) = (->)

instance (Category p, Op p ~ Y p) => Functor (Y p) where
  type Dom (Y p) = p
  type Cod (Y p) = Nat (Y p) (->)


{-
Given:  Category p, Op p ~ Y p

   -->  Category p, Op p ~ Y p
        Functor p, Dom p ~ Op p, Cod p ~ Nat p (->)

   -->  Category p, Op p ~ Y p
        Functor p, Dom p ~ Op p, Cod p ~ Nat p (->)
        Category (Dom p), Category (Cod p)
-}
