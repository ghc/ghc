{-# LANGUAGE DataKinds, PolyKinds #-}

module Dep1 where

import Data.Kind

data Proxy k (a :: k) = P

x :: Proxy Type Int
x = P

y :: Proxy Bool True
y = P
