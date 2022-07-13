{-# language PatternSignatureBinds #-}

module ShouldCompile where

import Data.Kind (Type)

data Psb (a :: c) :: b -> Type

data Psb2 (a :: c) :: forall b . b -> c -> Type
