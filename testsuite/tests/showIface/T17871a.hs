-- A.hs
module T17871a (C(..)) where

class C a where
  m :: a -> a
  m = identity

identity :: a -> a
identity x = x
