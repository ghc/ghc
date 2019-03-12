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

-- This code, supplied by Edward Kmett, uses UndecidableSuperClasses along
-- with a bunch of other stuff, so it's a useful stress test.
-- See #11480 comment:12.

module T11480b where

import Data.Kind (Constraint, Type)
import Data.Type.Equality as Equality
import Data.Type.Coercion as Coercion
import qualified Prelude
import Prelude (Either(..))

newtype Y (p :: i -> j -> Type) (a :: j) (b :: i) = Y { getY :: p b a }

type family Op (p :: i -> j -> Type) :: j -> i -> Type where
  Op (Y p) = p
  Op p = Y p

class Vacuous (p :: i -> i -> Type) (a :: i)
instance Vacuous p a

data Dict (p :: Constraint) where
  Dict :: p => Dict p

class Functor (Op p) (Nat p (->)) p => Category (p :: i -> i -> Type) where
  type Ob p :: i -> Constraint
  type Ob p = Vacuous p

  id :: Ob p a => p a a
  (.) :: p b c -> p a b -> p a c

  source :: p a b -> Dict (Ob p a)
  default source :: (Ob p ~ Vacuous p) => p a b -> Dict (Ob p a)
  source _ = Dict

  target :: p a b -> Dict (Ob p b)
  default target :: (Ob p ~ Vacuous p) => p a b -> Dict (Ob p b)
  target _ = Dict

  op :: p b a -> Op p a b
  default op :: Op p ~ Y p => p b a -> Op p a b
  op = Y

  unop :: Op p b a -> p a b
  default unop :: Op p ~ Y p => Op p b a -> p a b
  unop = getY

class (Category p, Category q) =>
      Functor (p :: i -> i -> Type)
              (q :: j -> j -> Type)
              (f :: i -> j) | f -> p q where
  fmap :: p a b -> q (f a) (f b)

data Nat (p :: i -> i -> Type)
         (q :: j -> j -> Type) (f :: i -> j) (g :: i -> j) where
  Nat :: (Functor p q f, Functor p q g) =>
         { runNat :: forall a. Ob p a => q (f a) (g a) } -> Nat p q f g

instance (Category p, Category q) => Category (Nat p q) where
  type Ob (Nat p q) = Functor p q
  id = Nat id1 where
    id1 :: forall f x. (Functor p q f, Ob p x) => q (f x) (f x)
    id1 = id \\ (ob :: Ob p x :- Ob q (f x))
  Nat f . Nat g = Nat (f . g)
  source Nat{} = Dict
  target Nat{} = Dict

ob :: forall p q f a. Functor p q f => Ob p a :- Ob q (f a)
ob = Sub (case source (fmap (id :: p a a) :: q (f a) (f a)) of Dict -> Dict)

instance (Category p, Category q) =>
         Functor (Y (Nat p q)) (Nat (Nat p q) (->)) (Nat p q) where
  fmap (Y f) = Nat (. f)

instance (Category p, Category q) => Functor (Nat p q) (->) (Nat p q f) where
  fmap = (.)

contramap :: Functor p q f => Op p b a -> q (f a) (f b)
contramap = fmap . unop

instance Category (->) where
  id = Prelude.id
  (.) = (Prelude..)

instance Functor (->) (->) ((->) e) where
  fmap = (.)

instance Functor (Y (->)) (Nat (->) (->)) (->) where
  fmap (Y f) = Nat (. f)

instance (Category p, Op p ~ Y p) => Category (Y p) where
  type Ob (Y p) = Ob p
  id = Y id
  Y f . Y g = Y (g . f)
  source (Y f) = target f
  target (Y f) = source f
  unop = Y
  op = getY

instance (Category p, Op p ~ Y p) => Functor (Y p) (->) (Y p a) where
  fmap = (.)

instance (Category p, Op p ~ Y p) => Functor p (Nat (Y p) (->)) (Y p) where
  fmap f = Nat (. Y f)

--------------------------------------------------------------------------------
-- * Constraints
--------------------------------------------------------------------------------

infixl 1 \\ -- comment required for cpp
(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r

newtype p :- q = Sub (p => Dict q)

instance Functor (:-) (->) Dict where
  fmap p Dict = case p of
    Sub q -> q

instance Category (:-) where
  id = Sub Dict
  f . g = Sub (Dict \\ f \\ g)

instance Functor (:-) (->) ((:-) e) where
  fmap = (.)

instance Functor (Y (:-)) (Nat (:-) (->)) (:-) where
  fmap (Y f) = Nat (. f)

--------------------------------------------------------------------------------
-- * Common Functors
--------------------------------------------------------------------------------

instance Functor (->) (->) ((,) e) where
  fmap f ~(a,b) = (a, f b)

instance Functor (->) (->) (Either e) where
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right (f b)

instance Functor (->) (->) [] where
  fmap = Prelude.fmap

instance Functor (->) (->) Prelude.Maybe where
  fmap = Prelude.fmap

instance Functor (->) (->) Prelude.IO where
  fmap = Prelude.fmap

instance Functor (->) (Nat (->) (->)) (,) where
  fmap f = Nat (\(a,b) -> (f a, b))

instance Functor (->) (Nat (->) (->)) Either where
  fmap f0 = Nat (go f0) where
    go :: (a -> b) -> Either a c -> Either b c
    go f (Left a)  = Left (f a)
    go _ (Right b) = Right b

--------------------------------------------------------------------------------
-- * Type Equality
--------------------------------------------------------------------------------

instance Category (:~:) where
  id = Refl
  (.) = Prelude.flip Equality.trans

instance Functor (Y (:~:)) (Nat (:~:) (->)) (:~:) where
  fmap (Y f) = Nat (. f)

instance Functor (:~:) (->) ((:~:) e) where
  fmap = (.)

--------------------------------------------------------------------------------
-- * Type Coercions
--------------------------------------------------------------------------------

instance Category Coercion where
  id = Coercion
  (.) = Prelude.flip Coercion.trans

instance Functor (Y Coercion) (Nat Coercion (->)) Coercion where
  fmap (Y f) = Nat (. f)

instance Functor Coercion (->) (Coercion e) where
  fmap = (.)
