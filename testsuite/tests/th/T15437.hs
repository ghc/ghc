{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
module T15437 where

import T15437A
import Data.Kind

f :: Int
f = $$(foo @Int)

g :: Int
g = $$(foo2 @Int)

h :: Int
h = $$(foo3 @Int)

i :: Int
i = $$(foo @(forall (a :: Type) . a -> a))

--a = $$(foo4 @Int)

b = $$(foo5 @Int @Bool)

s = $$(foo6 @Int)
