{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Overlap5 where

type family And (a :: Bool) (b :: Bool) :: Bool
type instance where
  And False x = False
  And True x = x
  And x False = False
  And x True = x
  And x x = x

data Proxy p = P

g :: Proxy x -> Proxy (And x True)
g x = x

h :: Proxy x -> Proxy (And x x)
h x = x

i :: Proxy x -> Proxy (And False x)
i x = (P :: Proxy False)