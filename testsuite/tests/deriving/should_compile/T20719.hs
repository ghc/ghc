{-# LANGUAGE DerivingStrategies, DeriveAnyClass, DefaultSignatures #-}

module T20719 where

import Data.Proxy

type Method a = forall b . Eq b => Proxy (a, b)

class Class a where
  method1         :: Method a
  default method1 :: Method a
  method1 = Proxy

  method2         :: Method a
  default method2 :: forall b . Eq b => Proxy (a, b)
  method2 = Proxy

  method3         :: forall b . Eq b => Proxy (a, b)
  default method3 :: Method a
  method3 = Proxy

  method4         :: forall b . Eq b => Proxy (a, b)
  default method4 :: forall b . Eq b => Proxy (a, b)
  method4 = Proxy

data Data
  deriving anyclass Class
