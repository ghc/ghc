module Main where

import Control.Concurrent

f :: Int -> Int
f x = x + 1

main :: IO ()
main = do
  m <- newEmptyMVar
  _ <- forkIO (print (f 1) >> putMVar m ())
  takeMVar m
  putStrLn "done"
