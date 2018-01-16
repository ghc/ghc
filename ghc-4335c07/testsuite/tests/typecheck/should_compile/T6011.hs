{-# LANGUAGE DataKinds, FlexibleInstances, ScopedTypeVariables, TypeFamilies #-}

module T6011 where

data family GenMod :: Modulus * -> * -> *

type Mod n = GenMod (FiniteRing n) Integer

data Modulus n = FiniteRing n

data instance GenMod (FiniteRing n) Integer = Mod Integer Integer
