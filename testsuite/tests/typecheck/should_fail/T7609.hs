{-# LANGUAGE TypeOperators #-}

module T7609 where

data X a b

f :: (a `X` a, Maybe)
f = undefined

g :: (a `X` a) => Maybe
g = undefined