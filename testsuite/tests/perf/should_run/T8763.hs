-- | The fusion helper for @enumFromThenTo \@Int@ had multiple
-- occurences of @c@, which made the simplifier refuse to inline it.
-- The new implementation for @efdtInt{Up,Dn}FB@ only have a single
-- occurence of @c@ which the simplifier inlines unconditionally.
module Main  (main) where

import Control.Monad (when, forM_)
import GHC.ST

nop :: Monad m => a -> m ()
nop _ = return ()
{-# NOINLINE nop #-}

-- This is the baseline, using @enumFromTo@ which already had only a
-- single occurence of @c@.
f :: Int -> ST s ()
f n =
    do
      forM_ [2..n] $ \p -> do
        let isPrime = p == (p - 1)
        when isPrime $
          forM_ [p + p, p + p + p .. n] $ \k ->  do
            nop k
{-# NOINLINE f #-}

g :: Int -> ST s ()
g n =
    do
      forM_ [2,3..n] $ \p -> do
        -- This do block should be too big to get inlined multiple times.
        -- Pad with @nop@s as necessary if this doesn't reproduce anymore.
        let isPrime = p == (p - 1)
        when isPrime $
          forM_ [p + p, p + p + p .. n] $ \k ->  do
            nop k
{-# NOINLINE g #-}

main :: IO ()
main = do
  -- runST (f 40000000) `seq` return ()
  runST (g 40000000) `seq` return ()
