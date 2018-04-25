{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Concurrent

haskellFun :: Int -> IO ()
haskellFun c = putStrLn ("Haskell: " ++ show c)

foreign export ccall "hFun"  haskellFun :: Int -> IO ()
foreign import ccall safe "hFun"  hFun :: Int -> IO ()

#ifdef mingw32_HOST_OS
foreign import stdcall safe "Sleep" _sleepBlock :: Int -> IO ()
sleepBlock n = _sleepBlock (n*1000)
#else
foreign import ccall safe "sleep" sleepBlock :: Int -> IO ()
#endif



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

