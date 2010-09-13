-- An example of RelaxedPolyRec in action which came up
-- on Haskell Cafe June 2010 (Job Vranish)

module Foo where

import Data.Maybe

-- The fixed point datatype
data Y f = Y (f (Y f))

-- Silly dummy function
maybeToInt :: Maybe a -> Int
maybeToInt = length . maybeToList

---------------------------
-- f and g are mutually recursive
-- Even though f has a totally monomorphic
-- signature, g has a very polymorphic one

f :: Y Maybe -> Int
f (Y x) = g maybeToInt x

-- With RelaxedPolyRec we can infer this type
-- g :: Functor f => (f Int -> b) -> f (Y Maybe) -> b
g h x = h $ fmap f x

-- 'test' checks that g's type is polymophic enough
test :: Functor f => (f Int -> b) -> f (Y Maybe) -> b
test = g
