module Foo where

f :: Show a => a
(f, _) = undefined

g :: Show a => a
g = f
