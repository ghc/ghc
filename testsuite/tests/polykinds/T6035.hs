{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, KindSignatures, GADTs,  TypeOperators
  #-}

module T6035 where

data Nat = Zero | Succ Nat

type family Sing (a :: k) :: k -> *

data SNat n where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

data SList (a :: [k]) where
  SNil :: SList '[]
  SCons :: Sing h h -> SList t -> SList (h ': t)

type instance Sing (a :: Nat) = SNat
type instance Sing (a :: [k]) = SList

nil :: SList '[]
nil = SNil

zero :: SList '[ '[] ]
zero = SCons SNil SNil

term :: SList '[ '[Zero], '[]]
term = SCons (SCons SZero SNil) (SCons SNil SNil)
