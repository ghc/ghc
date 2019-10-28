{-# LANGUAGE ScopedTypeVariables #-}

module Wpuns where
import Data.Proxy


data A = B
data T = T
data Just = J

-- -Wpuns, example #1
t = Proxy
type X = Proxy

-- -Wpuns, example #2
a = 15

h :: forall a. a -> a
h = \a -> (a :: a)

-- -Wpuns, example #3
b = 15

hh :: b -> b
hh = undefined

hhh :: a -> c -> a
hhh = undefined

-- Random examples

test :: m -> A
test x = B

f :: T
f = T

g :: x -> Just
g = undefined

ff c = undefined
  where
    g :: c -> c
    g = undefined
