{-# LANGUAGE GADTs, TypeFamilies #-}

module HardRecordUpdate where

import Data.Kind

type F :: Type -> Type
type family F a where {}

type G :: Type -> Type -> Type -> Type
data family G a b c
data instance G a b Char where
  MkG :: { bar :: F a } -> G a Bool Char

g :: F Int -> G Float b Char -> G Int b Char
g i r = r { bar = i }
