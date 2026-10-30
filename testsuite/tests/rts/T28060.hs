-- Test for #28060.
-- A GC that finds a dead weak pointer schedules its finalizer in a new
-- thread. That thread should run soon after the GC, at the next block
-- boundary of the thread that keeps running, not only at the next context
-- switch. The test runs under +RTS -C1, so that a finalizer left for the next
-- context switch waits up to a second, and counts the iterations of an
-- allocating loop between the GC and the finalizer's run: a few hundred at
-- most where the finalizer runs at a block boundary (one block's worth of
-- allocation), many more where it waits for the context switch.
--
-- The RTS timer requests its first context switch at its first tick, 10ms
-- after it starts, and then one every second under -C1. So the program first
-- allocates for 100ms, which takes it past that first switch, and only then
-- lets the weak pointer die.
module Main where

import Control.Monad (void)
import Data.IORef
import GHC.Clock (getMonotonicTimeNSec)
import System.Mem (performMajorGC)

{-# NOINLINE setup #-}
setup :: IORef Bool -> IO ()
setup flag = do
  r <- newIORef ()
  void $ mkWeakIORef r (writeIORef flag True)

-- Allocates until the monotonic clock reaches the given time.
allocateUntil :: Word -> IO ()
allocateUntil t = do
  _ <- newIORef t
  now <- fromIntegral <$> getMonotonicTimeNSec
  if now >= t then return () else allocateUntil t

-- Allocates an IORef an iteration, until the finalizer sets the flag
-- (or a bound, should it never run).
loop :: IORef Bool -> Int -> IO Int
loop flag !n
  | n >= 100000000 = return n
  | otherwise = do
      _ <- newIORef n
      done <- readIORef flag
      if done then return n else loop flag (n + 1)

main :: IO ()
main = do
  start <- fromIntegral <$> getMonotonicTimeNSec
  allocateUntil (start + 100000000)
  flag <- newIORef False
  setup flag
  performMajorGC
  n <- loop flag 0
  print (n < 1000)
