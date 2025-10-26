{-# LANGUAGE ExplicitNamespaces #-}

module T25901_imp_unused_2 where

import Data.Proxy (data ..)
import Data.Proxy (type Proxy, data ..)  -- only the wildcard is unused

p :: Proxy Int
p = Proxy