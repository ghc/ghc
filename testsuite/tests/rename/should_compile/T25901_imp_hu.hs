{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_hu where

import Data.Proxy as T hiding (data ..)
import Data.Proxy as D hiding (type ..)

f :: T.Proxy Int
f = D.Proxy

g :: Proxy Int
g = Proxy