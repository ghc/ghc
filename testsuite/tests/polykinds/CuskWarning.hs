{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds, GADTs, NoStarIsType, PolyKinds, TypeFamilies #-}
module CuskWarning where

import Data.Kind (Type)

type family TyEq (a :: k) (b :: l) :: Bool where
  TyEq a a = 'True
  TyEq a b = 'False

data Visible (k :: Type) :: k -> Type where
  MkVisible :: Visible (k1 -> k2) f -> Visible k1 a -> Visible k2 (f a)

data Nonrecursive (a :: k) = MkNonrecursive
data Inferred a = MkInferred
data Monomorphic (a :: Type) = MkMonomorphic
