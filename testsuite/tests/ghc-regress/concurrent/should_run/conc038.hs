module Main where

import Control.Concurrent

haskellFun :: Int -> IO ()
haskellFun c = putStrLn ("Haskell: " ++ show c)

foreign export "hFun"  haskellFun :: Int -> IO ()
foreign import "hFun"  safe hFun :: Int -> IO ()
foreign import "sleep" safe sleepBlock :: Int -> IO ()

main :: IO ()
main = do
  th <- newEmptyMVar
  forkIO $ do
     putStrLn "newThread started"
     sleepBlock 1
     putStrLn "newThread back again"
     putMVar th "1 sec later"
  threadDelay 500000 >> putStrLn "mainThread"
	-- this will not be blocked in the threaded RTS
  forkIO $ (hFun 2)
	-- neither will this
  x <- takeMVar th
  putStrLn x
  putStrLn "\nshutting down"

