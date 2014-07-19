{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module T9063 where

import Data.Type.Equality
import Data.Proxy

class kproxy ~ 'KProxy => PEq (kproxy :: KProxy a) where
  type (:==) (x :: a) (y :: a) :: Bool
  type x :== y = x == y

instance PEq ('KProxy :: KProxy Bool)

foo :: Proxy (True :== True) -> Proxy (True == True)
foo = id
