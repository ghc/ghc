{-# OPTIONS_GHC -O2 -fforce-recomp #-}
{-# language UnboxedTuples #-}

module T14816 where

import Data.Array.ST
import Control.Monad.ST.Strict
import Control.Monad

blink :: (a -> b) -> a -> (# b #)
blink g a = (# g a #)

test :: Int -> a -> (a -> a -> a) -> STArray s Int a -> ST s (STArray s Int a)
test k a f m = insertModifyingArr k (blink (f a)) m
{-# NOINLINE test #-}

-- The original program:
insertModifyingArr :: Int -> (a -> (# a #))
                   -> STArray s Int a -> ST s (STArray s Int a)
insertModifyingArr i0 f arr0 = do
   rng <- range <$> getBounds arr0
   go i0 rng arr0
  where
    go i [] arr = pure arr
    go i (k : ks) arr
      | i == k = do
          old <- readArray arr i
          case f old of (# new #) -> writeArray arr i new
          return arr
      | otherwise = go i ks arr

-- The following is inspired by $wpolynomial in simple,
-- see https://gitlab.haskell.org/ghc/ghc/-/issues/19001#note_340233.
-- TODO

