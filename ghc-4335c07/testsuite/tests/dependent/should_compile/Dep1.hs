{-# LANGUAGE TypeInType #-}

module Dep1 where

import Data.Kind

data Proxy k (a :: k) = P

x :: Proxy * Int
x = P

y :: Proxy Bool True
y = P
