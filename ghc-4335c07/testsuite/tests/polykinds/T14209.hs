{-# LANGUAGE TypeInType #-}
module T14209 where

data MyProxy k (a :: k) = MyProxy
data Foo (z :: MyProxy k (a :: k))
