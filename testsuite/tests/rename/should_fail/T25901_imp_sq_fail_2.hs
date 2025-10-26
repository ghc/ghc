{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_sq_fail_2 where

import qualified Data.Proxy as T (type ..)
import qualified Data.Proxy as D (data ..)

g :: Proxy Int
g = Proxy