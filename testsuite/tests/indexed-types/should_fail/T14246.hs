{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RankNTypes, GADTs, TypeOperators, PolyKinds, DataKinds, TypeFamilies, AllowAmbiguousTypes, UndecidableInstances #-}

module T14246 where

import Data.Kind

data Nat = Z | S Nat

data Vect :: Nat -> Type -> Type where
  Nil  :: Vect Z a
  Cons :: a -> Vect n a -> Vect (S n) a

data Label a = Label a

data L

type family KLN (n :: k) :: Nat where
    KLN (f :: v -> k) = S (KLN (forall t. f t))
    KLN (f :: Type) = Z

type family Reveal (n :: k) (l :: Vect (KLN n) L) :: Type where
    Reveal (f :: v -> k) (Cons (Label (t :: v)) l) = Reveal (f t) l
    Reveal (a :: Type) Nil = a
