{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, GADTs #-}

module Dep3 where

import Data.Kind
import GHC.Exts ( Constraint )

type Star1 = Type

data Id1 (a :: Star1) where
  Id1 :: a -> Id1 a

data Id1' :: Star1 -> Type where
  Id1' :: a -> Id1' a

type family Star2 x where
  Star2 x = Type

data Id2a (a :: Star2 Constraint) = Id2a a


data Id2 (a :: Star2 Constraint) where
  Id2 :: a -> Id2 a

data Id2' :: Star2 Constraint -> Type where
  Id2' :: a -> Id2' a
