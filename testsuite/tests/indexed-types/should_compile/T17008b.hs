{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T17008b where

import Data.Kind

type family ConstType1 (a :: Type) :: Type where
  ConstType1 _ = Type

type family F1 (x :: ConstType1 a) :: Type where
  F1 @a (x :: ConstType1 a) = a
type family F2 (x :: ConstType1 a) :: ConstType1 a where
  F2 @a (x :: ConstType1 a) = x :: ConstType1 a
type F3 (x :: ConstType1 a) = a
type F4 (x :: ConstType1 a) = x :: ConstType1 a

type ConstType2 (a :: Type) = Type

type family G1 (x :: ConstType2 a) :: Type where
  G1 @a (x :: ConstType2 a) = a
type family G2 (x :: ConstType2 a) :: ConstType2 a where
  G2 @a (x :: ConstType2 a) = x :: ConstType1 a
type G3 (x :: ConstType2 a) = a
type G4 (x :: ConstType2 a) = x :: ConstType2 a

type Id1 (a :: Type) = a

type family H (a :: Type) :: Type where
  H (Id1 a) = a
type family I (x :: Id1 a) :: Type where
  I (x :: Id1 a) = a

type family Id2 (a :: Type) :: Type where
  Id2 a = a

type family J (x :: Id2 a) :: Type where
  J (x :: Id2 a) = a
