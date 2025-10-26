{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_su_fail_1 where

import Data.Proxy as T (type ..)
import Data.Proxy as D (data ..)

h :: T.Proxy Int
h = T.Proxy