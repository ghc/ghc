module Main where

import Control.Concurrent
import System.IO.Unsafe

go :: MVar ()
go = unsafePerformIO newEmptyMVar
{-# NOINLINE go #-}

reached :: MVar ()
reached = unsafePerformIO newEmptyMVar
{-# NOINLINE reached #-}

-- Lets the worker run up to its breakpoint
slow :: Int
slow = unsafePerformIO $ do
  putMVar go ()
  takeMVar reached
  -- give the worker time to hit the breakpoint before `:force` completes (only
  -- bc. we used to unset the global break action after completing `:force`,
  -- and the worker breakpoint would hit the `noBreakAction`)
  threadDelay 100000
  pure 5
{-# NOINLINE slow #-}

inWorker :: Int -> IO ()
inWorker _ = pure ()

inMain :: Int -> IO ()
inMain _ = pure ()

main :: IO ()
main = do
  done <- newEmptyMVar
  _ <- forkIO (takeMVar go >> putMVar reached () >> inWorker 1 >> putMVar done ())
  inMain 0
  takeMVar done
  putStrLn "main done"
