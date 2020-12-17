{-# OPTIONS_GHC -O2 -fforce-recomp #-}
{-# LANGUAGE BangPatterns #-}

module T18174 (fac1, fac2, fac3, facIO, h1, h2) where

----------------------------------------------------------------------
-- First some basic examples that we want to CPR nestedly.

-- pretty strict
fac1 :: Int -> a ->  (a, Int)
fac1 n s | n < 2     = (s,1)
         | otherwise = case  fac1 (n-1) s of (s',n') -> let n'' = n*n' in n'' `seq` (s',n'')

-- lazier, but Int still has CPR
fac2 :: Int -> a ->  (a, Int)
fac2 n s | n < 2     = (s,1)
         | otherwise = case  fac2 (n-1) s of (s',n') -> (s',n'*n')

-- even lazier, but evaluation of the Int doesn't terminate rapidly!
-- Thus, we may not WW for the nested Int.
-- Otherwise @fac3 99999 () `seq` ()@ (which should terminate rapidly)
-- evaluates more than necessary.
fac3 :: Int -> a ->  (a, Int)
fac3 n s | n < 2     = (s,1)
         | otherwise = let (s',n') = fac3 (n-1) s in (s',n'*n')

facIO :: Int -> IO Int
facIO n | n < 2     = return 1
        | otherwise = do n' <- facIO (n-1); return (n*n')

----------------------------------------------------------------------
-- The following functions are copied from T18894. This test is about
-- *exploiting* the demand signatures that we assertedly (by T18894)
-- annotate.

g1 :: Int -> (Int,Int)
g1 1 = (15, 0)
g1 n = (2 * n, 2 `div` n)
{-# NOINLINE g1 #-}

-- | Sadly, the @g1 2@ subexpression will be floated to top-level, where we
-- don't see the specific demand placed on it by @snd@. Tracked in #19001.
h1 :: Int -> Int
h1 1 = 0
h1 2 = snd (g1 2)
h1 m = uncurry (+) (g1 m)

-- | So @g2@ here takes an additional argument m that prohibits floating to
-- top-level. We want that argument to have the CPR property, so we have
-- to add a bang so that it's used strictly and ultimately unboxed.
-- We expect the following CPR type:
--
-- > #c1(#c1(#), *c1(#))
--
-- In combination with the the fact that all calls to @g2@ evaluate the second
-- component of the pair, we may unbox @g2@ to @(# Int#, Int# #)@.
g2 :: Int -> Int -> (Int,Int)
g2 !m 1 = (2 + m, 0)
g2 m  n = (2 * m, 2 `div` n)
{-# NOINLINE g2 #-}

h2 :: Int -> Int
h2 1 = 0
h2 m
  | odd m     = snd (g2 m 2)
  | otherwise = uncurry (+) (g2 2 m)
