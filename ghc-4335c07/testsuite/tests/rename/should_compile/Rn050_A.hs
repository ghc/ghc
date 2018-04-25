module Rn050_A where

{-# DEPRECATED C "Use D instead" #-}
{-# DEPRECATED op "Use bop instead" #-}

data T = C | D

class Foo a where
  op :: a -> a
  bop :: a -> a
