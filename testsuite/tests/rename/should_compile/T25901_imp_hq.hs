{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_hq where

import qualified Data.Proxy as T hiding (data ..)
import qualified Data.Proxy as D hiding (type ..)

f :: T.Proxy Int
f = D.Proxy