{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies,
             TypeOperators, RankNTypes #-}

module T7294 where

import Data.Kind (Type)

data Nat = Zero | Succ Nat

data Vec :: Type -> Nat -> Type where
  Nil :: Vec a Zero
  Cons :: a -> Vec a n -> Vec a (Succ n)

type family (m :: Nat) :< (n :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance Succ n :< Succ m = n :< m

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: forall (n :: Nat). SNat n -> SNat (Succ n)

nth :: ((k :< n) ~ True) => Vec a n -> SNat k -> a
nth (Cons x _) SZero = x
nth (Cons _ xs) (SSucc k) = nth xs k
nth Nil _ = undefined
