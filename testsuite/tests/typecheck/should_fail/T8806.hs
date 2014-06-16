-- Trac #8806

module T8806 where

f :: Int => Int
f x = x + 1

g :: (Int => Show a) => Int
g = undefined
