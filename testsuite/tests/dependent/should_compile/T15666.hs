{-# LANGUAGE
  NoImplicitPrelude,
  PolyKinds, DataKinds,
  ScopedTypeVariables,
  TypeFamilies,
  UndecidableInstances
#-}

module T15666 where

import Data.Kind(Type)

data PolyType k (t :: k)

type Wrap (t :: k) = PolyType k t
type Unwrap pt = (GetType pt :: GetKind pt)

type family GetKind (pt :: Type) :: Type where
  GetKind (PolyType k t) = k

type GetType :: Type -> k
type family GetType (pt :: Type) :: k where
  GetType @k (PolyType k t) = t

data Composite :: a -> b -> Type

type family RecursiveWrap expr where
  RecursiveWrap (Composite @Type @Type a b) =
    Wrap (Composite (Unwrap (RecursiveWrap a)) (Unwrap (RecursiveWrap b)))
  RecursiveWrap x = Wrap x
