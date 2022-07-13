{-# language PatternSignatureBinds #-}

module ShouldCompile where

import Data.Kind (Type)

data Psb (a :: c) :: b -> Type

data Psb2 (a :: c) :: forall b . b -> c -> Type

type family F (a :: k) :: k

bad :: forall a b . (a -> b) -> F a -> F b
bad _ = undefined
