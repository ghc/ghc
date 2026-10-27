module Main where

import Control.Concurrent
import Control.Monad

midpoint :: Int -> IO ()
midpoint _ = pure ()

worker :: MVar () -> MVar () -> MVar () -> Int -> IO ()
worker turn next done n = do
  takeMVar turn
  putMVar next ()
  midpoint n
  putMVar done ()

main :: IO ()
main = do
  turns@(first : rest) <- replicateM 4 newEmptyMVar
  done <- newEmptyMVar
  forM_ (zip3 turns (rest ++ [first]) [1 ..]) $ \(turn, next, n) ->
    forkIO (worker turn next done n)
  putMVar first ()
  replicateM_ 4 (takeMVar done)
  putStrLn "main done"
