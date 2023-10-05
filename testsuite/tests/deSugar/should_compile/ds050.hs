{-# LANGUAGE RankNTypes #-}

module ShouldCompile where

data Q = Q {f :: forall a. a -> a}
g1 = f
g2 x = f x
g3 x y = f x y
