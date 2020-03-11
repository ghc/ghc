{-# LANGUAGE PolyKinds #-}

module T17841 where

data Proxy a = Proxy

class Foo (t :: k) where foo :: Proxy (a :: t)
