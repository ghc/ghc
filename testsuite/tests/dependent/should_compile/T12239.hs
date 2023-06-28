{-# LANGUAGE PolyKinds, DataKinds, GADTs, TypeFamilies #-}

module T12239 where

import Data.Kind (Type)

data N = Z | S N

data Fin :: N -> Type where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

type family FieldCount (t :: Type) :: N

type family FieldType (t :: Type) (i :: Fin (FieldCount t)) :: Type

data T

type instance FieldCount T = S (S (S Z))

type instance FieldType T FZ = Int
type instance FieldType T (FS FZ) = Bool
type instance FieldType T (FS (FS FZ)) = String
