{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Overlap12 where

type family And (a :: Bool) (b :: Bool) :: Bool
type instance where
  And False x = False
  And True x = x
  And x False = False
  And x True = x
  And x x = x

data Proxy p = P

i :: Proxy (And False x)
i = (P :: Proxy False)