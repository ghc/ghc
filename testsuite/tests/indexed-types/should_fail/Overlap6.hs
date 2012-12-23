{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Overlap6 where

type family And (a :: Bool) (b :: Bool) :: Bool
type instance where
  And False x = False
  And True x = False -- this is wrong!
  And x False = False
  And x True = x
  And x x = x

data Proxy p = P

g :: Proxy x -> Proxy (And x True)
g x = x
