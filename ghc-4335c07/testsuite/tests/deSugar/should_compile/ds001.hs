-- !!! ds001 -- simple function and pattern bindings
--
-- this tests ultra-simple function and pattern bindings (no patterns)

module ShouldCompile where

-- simple function bindings

f x = x

g x y z = f z

j w x y z = g w x z

h x y = f y
  where
    f a b = a

-- simple pattern bindings

a = b

b = f

c = c
