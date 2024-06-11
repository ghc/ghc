{-# LANGUAGE GHC2021 #-}

module T24845a where

import GHC.TypeLits
import Data.Proxy

f :: (a+b ~ a+c) => Proxy b -> Proxy c
f x = x   -- Should be accepted

g :: (a+b ~ d, a+c ~ d) => Proxy b -> Proxy c
g x = x   -- Should be accepted
