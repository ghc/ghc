{-# LANGUAGE ExplicitNamespaces #-}

module T25901_sub_a where

import Data.Proxy (Proxy (type ..))  -- should not bring the data constructor Proxy into scope

p :: Proxy String
p = Proxy  -- error: the data constructor is not in scope