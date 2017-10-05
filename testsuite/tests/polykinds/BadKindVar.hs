{-# LANGUAGE RankNTypes, KindSignatures #-}

module Foo where

import Data.Proxy

-- Should be illegal without PolyKinds
f :: forall (a :: k). Proxy a
f = f
