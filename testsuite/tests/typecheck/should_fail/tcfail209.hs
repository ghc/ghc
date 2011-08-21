module FancyContextsWithoutExtension1 where

type Showish = Show

f :: (Showish a) => a -> a
f = undefined

g :: ((Show a, Num a), Eq a) => a -> a
g = undefined
