{-# OPTIONS -fglasgow-exts #-}

module Main where

import Control.Concurrent
import Control.Exception

choose :: a -> a -> IO a
choose a b = do
   ready <- newMVar ()
   answer <- newEmptyMVar
   a_id <- myForkIO (a `seq` takeMVar ready >> putMVar answer a)
   b_id <- myForkIO (b `seq` takeMVar ready >> putMVar answer b)
   it <- takeMVar answer
   killThread a_id
   killThread b_id
   return it

-- We need to catch the exception raised by killThread and ignore it.
-- Otherwise the default handler will exit the program when this
-- exception is raised in any thread.

myForkIO :: IO () -> IO ThreadId
myForkIO io = forkIO (Control.Exception.catch io (\e -> return ()))

main = do
   let big = sum [1..]
       small = sum [1..42]
   test1 <- choose big small
   test2 <- choose small big
   print (test1,test2)
