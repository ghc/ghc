module Main where

import Control.Concurrent
import Control.Monad

midpoint :: Int -> IO ()
midpoint _ = pure ()

worker :: MVar () -> IO ()
worker done = do
  forM_ [1 .. 3] $ \i -> do
    threadDelay 1000
    midpoint i
  putMVar done ()

main :: IO ()
main = do
  dones <- forM [1 .. 4 :: Int] $ \_ -> do
    done <- newEmptyMVar
    _ <- forkIO (worker done)
    pure done
  mapM_ takeMVar dones
  putStrLn "all workers done"
