module Main where

import Control.Concurrent
import System.IO.Unsafe

go :: MVar ()
go = unsafePerformIO newEmptyMVar
{-# NOINLINE go #-}

slow :: Int
slow = unsafePerformIO $ do
  putMVar go ()
  threadDelay 200000
  pure 5
{-# NOINLINE slow #-}

inWorker :: Int -> IO ()
inWorker _ = pure ()

inMain :: Int -> IO ()
inMain _ = pure ()

main :: IO ()
main = do
  done <- newEmptyMVar
  _ <- forkIO (takeMVar go >> inWorker 1 >> putMVar done ())
  inMain 0
  takeMVar done
  putStrLn "main done"
