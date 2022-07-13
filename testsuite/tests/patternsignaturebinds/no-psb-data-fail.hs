{-# language NoPatternSignatureBinds #-}

module ShouldFail where

import Data.Kind (Type)

data Psb (a :: c) :: b -> Type
