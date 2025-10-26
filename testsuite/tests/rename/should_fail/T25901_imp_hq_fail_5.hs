{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_hq_fail_5 where

import qualified Data.Proxy as T hiding (data ..)
import qualified Data.Proxy as D hiding (type ..)

g :: Proxy Int
g = Proxy