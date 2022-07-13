{-# language ImplicitForAll #-}

module ShouldCompile where

import Data.Kind (Type)

data Imp1 a :: b -> Type

data Imp2 a :: forall b . b -> c -> Type
