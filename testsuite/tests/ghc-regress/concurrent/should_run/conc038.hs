module Main where

import Control.Concurrent

haskellFun :: Int -> IO ()
haskellFun c = putStrLn ("Haskell: " ++ show c)

foreign export "hFun" haskellFun :: Int -> IO ()
foreign import "hFun" threadsafe hFun :: Int -> IO ()
foreign import "sleep" threadsafe sleepBlock :: Int -> IO ()

main :: IO ()
main = do
  th <- newEmptyMVar
  forkIO $ do
     putStrLn "newThread started"
     sleepBlock 1
     putStrLn "newThread back again"
     putMVar th "1 sec later"
  yield -- make sure the newly created thread is run.
  forkIO $ (hFun 2)
  putStrLn "mainThread"
  x <- takeMVar th
  putStrLn x
  putStrLn "\nshutting down"
  
