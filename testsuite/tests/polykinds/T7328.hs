{-# LANGUAGE PolyKinds, GADTs, AllowAmbiguousTypes #-}

module T7328 where

data Proxy a

class Foo a where
    foo :: a ~ f i => Proxy (Foo f)
