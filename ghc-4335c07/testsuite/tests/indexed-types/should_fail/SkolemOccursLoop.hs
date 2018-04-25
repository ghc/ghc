{-# OPTIONS_GHC -ftype-function-depth=3 #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, EmptyDataDecls #-}

module SkolemOccursLoop where

-- SkelemOccurs tests by Tom and Martin

data T x
type family F x
type instance F [x] = [T (F x)]

t :: a -> a -> Bool
t _ _ = True

f :: a -> F [a]
f = undefined

test1 :: (F [a] ~ a) => a -> Bool
test1 x = t x (f x)

--

data S a
type family G x
type instance G (S x, y) = S (G (x,y))

g :: a -> G [a]
g = undefined

test2 :: (G (S a,a) ~ a) => a -> Bool
-- inferred: G [a] ~ a => a -> Bool
test2 x = t x (g x)
