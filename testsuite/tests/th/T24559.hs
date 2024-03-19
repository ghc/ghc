{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
module Foo where

import Data.Kind
import Data.Proxy

f :: (forall (a :: Type). Proxy a) -> Proxy Bool
f k = k @Bool

g1 :: Proxy Bool
g1 = f (\ @a -> Proxy @a)

g2 :: Proxy Bool
g2 = f $([| \ @a -> Proxy @a |])
