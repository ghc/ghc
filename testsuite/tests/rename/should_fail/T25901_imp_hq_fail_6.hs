{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_hq_fail_6 where

import qualified Data.Proxy as T hiding (data ..)
import qualified Data.Proxy as D hiding (type ..)

h :: T.Proxy Int
h = T.Proxy