-- Check that splicing in a quoted declaration has the same effect as
-- giving the declaration directly.
{-# LANGUAGE TemplateHaskell, TypeData, GADTs #-}

module TD_TH_splice where

import Data.Kind (Type)

-- splice should be equivalent to giving the declaration directly
$( [d| type data Nat = Zero | Succ Nat |] )

data Vec :: Nat -> Type -> Type where
    VNil :: Vec Zero a
    VCons :: a -> Vec n a -> Vec (Succ n) a

instance Functor (Vec n) where
    fmap _ VNil = VNil
    fmap f (VCons x xs) = VCons (f x) (fmap f xs)
