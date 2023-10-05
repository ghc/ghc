{-# LANGUAGE TypeData #-}
{-# LANGUAGE MonoLocalBinds #-}
module TDVector where

import Data.Kind (Type)

type data Nat = Zero | Succ Nat

type data List a = Nil | Cons a (List a)

type data Pair a b = MkPair a b

type data Sum a b = L a | R b

data Vec :: Nat -> Type -> Type where
    VNil :: Vec Zero a
    VCons :: a -> Vec n a -> Vec (Succ n) a

instance Functor (Vec n) where
    fmap _ VNil = VNil
    fmap f (VCons x xs) = VCons (f x) (fmap f xs)
