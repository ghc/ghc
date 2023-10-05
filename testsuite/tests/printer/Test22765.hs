{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}

module Test22765 where

import Data.Kind (Type)
import Data.Type.Equality

-- example from GHC User's Guide 6.4.10.6

type data Ex :: Type where
  MkEx :: forall a. a -> Ex

type family UnEx (ex :: Ex) :: k
type instance UnEx (MkEx x) = x

-- -------------------------------------

type data P = MkP
data Prom = P

-- -------------------------------------

type data Nat = Zero | Succ Nat

-- type level GADT
type data Vec :: Nat -> Type -> Type where
    VNil :: Vec Zero a
    VCons :: a -> Vec n a -> Vec (Succ n) a

type X = VCons Bool (VCons Int VNil)

-- -------------------------------------

type data Foo :: Type -> Type where
  MkFoo1 :: a ~ Int         => Foo a
  MkFoo2 :: a ~~ Int        => Foo a

-- -------------------------------------

-- splice should be equivalent to giving the declaration directly
$( [d| type data Nat = Zero | Succ Nat |] )

data Vec :: Nat -> Type -> Type where
    VNil :: Vec Zero a
    VCons :: a -> Vec n a -> Vec (Succ n) a

instance Functor (Vec n) where
    fmap _ VNil = VNil
    fmap f (VCons x xs) = VCons (f x) (fmap f xs)

-- -------------------------------------

type data List a = Nil | Cons a (List a)

type data Pair a b = MkPair a b

type data Sum a b = L a | R b
