{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, RankNTypes #-}

module SAKS_032 where

import Data.Kind
import Data.Proxy

type Const :: Type -> forall k. k -> Type
data Const a b = Const a

type F :: Type -> Type -> forall k. k -> Type
type family F a b :: forall k. k -> Type where
  F () () = Proxy
  F a b = Const (a,b)

type F1 :: Type -> forall j. j -> forall k1 k2. (k1, k2) -> Type
type family F1 a b

type F2 :: Type -> forall j. j -> forall k1 k2. (k1, k2) -> Type
type family F2 a b :: forall r2. (r1, r2) -> Type
