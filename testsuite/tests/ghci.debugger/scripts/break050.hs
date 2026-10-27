module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad

f :: Int -> IO ()
f x = do
  when (x == 1) $ writeFile "break050.started" ""
  threadDelay 1000
  f (x+1)

main :: IO ()
main = do
  done <- newEmptyMVar
  _ <- forkIO $ f 0 `catch` \e -> do
    putStrLn ("forked thread: " ++ show (e :: AsyncException))
    putMVar done ()
  takeMVar done
  putStrLn "main was not interrupted"
