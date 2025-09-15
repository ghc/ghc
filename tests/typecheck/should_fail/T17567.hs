{-# LANGUAGE PolyKinds, RankNTypes #-}

module T17567 where

import Data.Proxy

type T = forall a. Proxy a

p :: T
p = Proxy

x :: Proxy a -> a
x = undefined

y = x p
