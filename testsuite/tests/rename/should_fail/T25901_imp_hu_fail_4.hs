{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_hu_fail_4 where

import Data.Proxy as T hiding (data ..)
import Data.Proxy as D hiding (type ..)

h :: T.Proxy Int
h = T.Proxy