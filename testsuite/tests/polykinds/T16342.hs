{-# LANGUAGE MultiParamTypeClasses, TypeInType, ConstrainedClassMethods, ScopedTypeVariables #-}

module T16342 where

import Data.Proxy

class C (a::ka) x where
  cop :: D a x => x -> Proxy a -> Proxy a
  cop _ x = x :: Proxy (a::ka)

class D (b::kb) y where
  dop :: C b y => y -> Proxy b -> Proxy b
  dop _ x = x :: Proxy (b::kb)
