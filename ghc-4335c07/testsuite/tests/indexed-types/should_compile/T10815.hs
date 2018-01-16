{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}

module T10815 where

import Data.Proxy

type family Any :: k

class kproxy ~ 'KProxy => C (kproxy :: KProxy k) where
  type F (a :: k)
  type G a :: k

instance C ('KProxy :: KProxy Bool) where
  type F a = Int
  type G a = Any
