{-# LANGUAGE RankNTypes, GADTs, TypeOperators, PolyKinds, DataKinds, TypeFamilies, AllowAmbiguousTypes, UndecidableInstances, TypeInType #-}

module T14246 where

import Data.Kind -- necessary for *

data Nat = Z | S Nat

data Vect :: Nat -> Type -> Type where
  Nil  :: Vect Z a
  Cons :: a -> Vect n a -> Vect (S n) a

data Label a = Label a

data L

type family KLN (n :: k) :: Nat where
    KLN (f :: v -> k) = S (KLN (forall t. f t))
    KLN (f :: *) = Z

type family Reveal (n :: k) (l :: Vect (KLN n) L) :: * where
    Reveal (f :: v -> k) (Cons (Label (t :: v)) l) = Reveal (f t) l
    Reveal (a :: *) Nil = a
