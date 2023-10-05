{-# OPTIONS_GHC -O2 -fforce-recomp #-}

-- | The point of this test is that @g*@ get's a demand that says
-- "whenever @g*@ is called, the second component of the pair is evaluated strictly".
module T18894 (h1, h2) where

g1 :: Int -> (Int,Int)
g1 1 = (15, 0)
g1 n = (2 * n, 2 `div` n)
{-# NOINLINE g1 #-}

h1 :: Int -> Int
h1 1 = 0
-- Sadly, the @g1 2@ subexpression will be floated to top-level, where we
-- don't see the specific demand placed on it by @snd@. Tracked in #19001.
h1 2 = snd (g1 2)
h1 m = uncurry (+) (g1 m)

g2 :: Int -> Int -> (Int,Int)
g2 m 1 = (m, 0)
g2 m n = (2 * m, 2 `div` n)
{-# NOINLINE g2 #-}

h2 :: Int -> Int
h2 1 = 0
h2 m
  | odd m     = snd (g2 m 2)
  | otherwise = uncurry (+) (g2 2 m)
