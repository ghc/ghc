module Main where

import Control.Concurrent

inWorker :: Int -> IO ()
inWorker _ = pure ()

inMain :: Int -> IO ()
inMain _ = pure ()

main :: IO ()
main = do
  done <- newEmptyMVar
  _ <- forkIO (threadDelay 100000 >> inWorker 1 >> putMVar done ())
  inMain 0
  takeMVar done
  putStrLn "main done"
