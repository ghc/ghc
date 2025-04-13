{-# LANGUAGE DataKinds, TypeFamilies #-}

module T12088g where

import Data.Kind (Type)

data N = Z | S N

data Fin :: N -> Type where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

type family FieldCount (t :: Type) :: N

type family FieldType (t :: Type) (i :: Fin (FieldCount t)) :: Type

newtype Field (t :: Type) (i :: Fin (FieldCount t)) = Field (FieldType t i)

data L

type instance FieldCount L = S Z
type instance FieldType L FZ = ()

l_fz :: FieldType L FZ
l_fz = ()

field_l_fz  :: Field L FZ
field_l_fz = Field l_fz

