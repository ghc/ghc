{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeOperators, GADTs, MultiParamTypeClasses, FlexibleContexts #-}
-- | This documentation refers to '~', ':-:', ':+:', '<=>', and '|||'.
module TypeOperators where

data a :-: b

data (a :+: b) c

data a `Op` b

newtype (g `O` f) a = O { unO :: g (f a) }

class a <=> b

type (|||) = Either

biO :: (g `O` f) a
biO = undefined

f :: (a ~ b) => a -> b
f = id

g :: (a ~ b, b ~ c) => a -> c
g = id

x :: ((a :-: a) <=> (a `Op` a)) => a
x = undefined

y :: (a <=> a, (a `Op` a) <=> a) => a
y = undefined
