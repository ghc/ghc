{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module T9063 where

import Data.Type.Equality
import Data.Proxy

-- reproduces an issue where type variables in the axiom are in
-- non-deterministic order

class kproxy ~ 'KProxy => PEq (kproxy :: KProxy a) where
  type FunnyEq (x :: a) (y :: a) :: Bool
  type FunnyEq x y = x == y

instance PEq ('KProxy :: KProxy Bool)

foo :: Proxy (FunnyEq True True) -> Proxy (True == True)
foo = id
