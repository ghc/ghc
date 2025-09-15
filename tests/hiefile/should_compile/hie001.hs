module Foo where

data Foo = Bar { x :: Int, y :: Bool }

foo a = a{x=2}

bar = Bar{ x = 1, y = False}

foobar = foo (id id bar)
