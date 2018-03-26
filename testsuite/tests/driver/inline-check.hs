module InlineCheck where

foo = (+1)

foo1 = (+1)
{-# NOINLINE foo1 #-}

qux = foo 3

qux1 = foo1 3

too = qux
