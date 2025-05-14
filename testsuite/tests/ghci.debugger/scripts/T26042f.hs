module T8 where

import Data.Functor.Identity

top = runIdentity $ f 15
f :: Int -> Identity Int
f x = do
  b <- g (x*x)
  y <- pure (b+b)
  return (y+y)

g :: Int -> Identity Int
g x = do
  a <- t (x*2)
  n <- pure (a+a)
  return (n+n)
{-# OPAQUE g #-}

t :: Int -> Identity Int
t x = do
  pure (x + 3)
{-# OPAQUE t #-}

