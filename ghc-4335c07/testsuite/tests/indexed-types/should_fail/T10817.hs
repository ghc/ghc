{-# LANGUAGE TypeFamilies #-}

module T10817 where

import Data.Proxy

class C a where
  type F a
  type F a = F a

instance C Bool

x :: Proxy (F Bool)
x = Proxy
