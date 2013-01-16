{-# LANGUAGE GADTs, ConstraintKinds, TypeFamilies, 
    DataKinds, ScopedTypeVariables, TypeOperators #-}

module T7090 where

import GHC.Exts

data Dict c where
  Dict :: c => Dict c

data Nat = Zero | Succ Nat

type family Plus (a :: Nat) (b :: Nat) :: Nat
type instance Plus Zero b = b
type instance Plus (Succ a) b = Succ (Plus a b)

type One = Succ Zero

type family (a :: Nat) :==: (b :: Nat) :: Bool

boolToProp :: (a :==: b) ~ True => Dict (a ~ b)
boolToProp = undefined

data T (n :: Nat) = MkT

foo :: forall n. (Succ n :==: Plus n One) ~ True => T n
foo = case (boolToProp :: Dict (Succ n ~ Plus n One)) of
           Dict -> MkT
