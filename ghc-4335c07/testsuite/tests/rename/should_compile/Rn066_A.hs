module Rn066_A where

{-# WARNING C "Are you sure you want to do that?" #-}
{-# WARNING op "Is that really a good idea?" #-}

data T = C | D

class Foo a where
  op :: a -> a
  bop :: a -> a
