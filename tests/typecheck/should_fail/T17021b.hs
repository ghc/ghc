{-# LANGUAGE PolyKinds, TypeFamilies, DataKinds #-}

module T17021b where

import Data.Kind

data family Fix (f :: Type -> k) :: k
type family F (a :: Type) :: Type where
  F Int = Type -> Type
data instance Fix (f :: Type -> F Int) :: F Int
