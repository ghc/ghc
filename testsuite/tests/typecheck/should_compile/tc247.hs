{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module ShouldCompile where

import Data.Kind (Type)

-- Various forms of empty data type declarations

data T1

data T2 where

data T3 :: Type -> Type

data T4 a :: Type -> Type

data T5 a :: Type -> Type where


