{-# LANGUAGE TypeApplications #-}

module T21530a where

import Data.Kind
import Data.Proxy

f :: forall (a :: Type). Proxy a -> Int
f = f

g :: Proxy (Eq Int)
g = g

h = f g
