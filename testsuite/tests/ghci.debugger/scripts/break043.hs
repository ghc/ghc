module Main where

import Control.Concurrent

inWorker :: Int -> IO ()
inWorker _ = pure ()

inMain :: Int -> IO ()
inMain _ = pure ()

main :: IO ()
main = do
  done <- newEmptyMVar
  _ <- forkIO (inWorker 1 >> putMVar done ())
  takeMVar done
  inMain 2
  putStrLn "main done"
