{-# LANGUAGE TypeOperators, BangPatterns #-}

module T2387 (mainLoop) where

import Control.Monad.ST

import System.Environment

data (:*:) a b = !a :*: !b

whileLoop :: Int -> ST s Int
whileLoop = go 0
 where
 go !n k
   | k == 0    = return n
   | otherwise = go (n+1) (k-1)
{-# INLINE whileLoop #-}

iter :: Int -> Int -> ST s (Bool :*: Int)
iter k n = do
  k' <- whileLoop 40 >>= \k' -> return $! max k k'
  b <- return (n == 0)

  return $! b :*: k'
{-# INLINE iter #-}

-- | The returned Int should be unboxed
mainLoop :: Int -> Int -> ST s Int
mainLoop k n = do
  done :*: k' <- iter k n

  if done
    then return k'
    else mainLoop k' (n - 1)
