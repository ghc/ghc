{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_su where

import Data.Proxy as T (type ..)
import Data.Proxy as D (data ..)

f :: T.Proxy Int
f = D.Proxy

g :: Proxy Int
g = Proxy