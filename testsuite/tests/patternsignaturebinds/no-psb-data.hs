{-# language NoPatternSignatureBinds #-}

module ShouldCompile where

import Data.Kind (Type)

data Psb (a :: c) :: b -> c -> Type
