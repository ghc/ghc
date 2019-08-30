module MultipleNoinlines where

foo_x = 1
foo_y = 2
foo_z = 3

t = foo_x + foo_y + foo_z

{-# NOINLINE foo_x, foo_y, foo_z #-}
