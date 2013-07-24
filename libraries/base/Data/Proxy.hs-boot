{-# LANGUAGE NoImplicitPrelude, PolyKinds #-}

module Data.Proxy ( Proxy(..) ) where

data Proxy (t :: k) = Proxy