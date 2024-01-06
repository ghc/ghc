module T18907 (m, f, g, h) where

import Control.Monad (forever)
import Control.Monad.Trans.State.Strict

inc :: State Int ()
inc = modify' (+1)

m :: State Int ()
m = forever inc

data Huge = H Int Int Int Int Int

-- | Should not unbox `x`.
f :: Huge -> Huge
f !x
  | sum [0..24::Int] == 1 = x
  | otherwise             = H 0 0 0 0 0
{-# NOINLINE f #-}

-- | Should not unbox `x`.
g :: Huge -> Huge
g x@(H a b c d e) = a `seq` x
{-# NOINLINE g #-}

seq' a b = seq a b
{-# NOINLINE seq' #-}

-- | Should not unbox `y`. Unboxing `x` is OK.
h :: Int -> Int -> Int
h x y = (x+1) `seq'` y
{-# NOINLINE h #-}
