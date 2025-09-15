{-# LANGUAGE DataKinds, FlexibleInstances, ScopedTypeVariables, TypeFamilies #-}

module T6011 where

import Data.Kind (Type)

data family GenMod :: Modulus Type -> Type -> Type

type Mod n = GenMod (FiniteRing n) Integer

data Modulus n = FiniteRing n

data instance GenMod (FiniteRing n) Integer = Mod Integer Integer
