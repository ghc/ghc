module Foo where

-- It's easy to get an infinite loop
-- when deciding what to unbox here.

data T = MkT !S Int
data S = MkS !T