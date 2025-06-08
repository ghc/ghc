{-# LANGUAGE RankNTypes, GADTs, TypeOperators, PolyKinds, DataKinds, TypeFamilies, AllowAmbiguousTypes, UndecidableInstances #-}

module T14246 where

import Data.Kind

data Nat = Z | S Nat

type Vect :: Nat -> Type -> Type
data Vect n a where
  Nil  :: Vect Z a
  Cons :: a -> Vect n a -> Vect (S n) a

data Label a = Label a

data L

type KLN :: k -> Nat
type family KLN n where
    KLN (f :: v -> Type) = S (KLN (forall t. f t))
    KLN (f :: Type) = Z

type Reveal :: forall k. forall (n :: k) -> Vect (KLN n) L -> Type
type family Reveal n l where
    Reveal (f :: v -> k) (Cons (Label (t :: v)) l) = Reveal (f t) l
    Reveal (a :: Type) Nil = a
