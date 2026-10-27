module Main where

import Control.Concurrent
import Control.Monad
import Data.IORef
import System.IO.Unsafe

ticks :: IORef Int
ticks = unsafePerformIO (newIORef 0)
{-# NOINLINE ticks #-}

f :: Int -> IO ()
f x = do
  when (x == 1) $ writeFile "break050.started" ""
  modifyIORef' ticks (+1)
  threadDelay 1000
  f (x+1)

main :: IO ()
main = do
  _ <- forkIO (f 0)
  threadDelay 2000000
  putStrLn "main was not interrupted"
