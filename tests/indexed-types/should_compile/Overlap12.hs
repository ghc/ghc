{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Overlap12 where

type family And (a :: Bool) (b :: Bool) :: Bool where
  And False x = False
  And True x = x
  And x False = False
  And x True = x
  And x x = x

data Proxy p = P

a :: Proxy (And False x)
a = (P :: Proxy False)

b :: Proxy x -> Proxy (And True x)
b x = x

c :: Proxy (And x False)
c = (P :: Proxy False)

d :: Proxy x -> Proxy (And x True)
d x = x

e :: Proxy x -> Proxy (And x x)
e x = x
