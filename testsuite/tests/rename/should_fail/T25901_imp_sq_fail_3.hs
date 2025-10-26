{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_sq_fail_3 where

import qualified Data.Proxy as T (type ..)
import qualified Data.Proxy as D (data ..)

h :: T.Proxy Int
h = T.Proxy