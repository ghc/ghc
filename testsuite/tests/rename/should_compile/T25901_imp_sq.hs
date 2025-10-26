{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_sq where

import qualified Data.Proxy as T (type ..)
import qualified Data.Proxy as D (data ..)

f :: T.Proxy Int
f = D.Proxy