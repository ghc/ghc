
module Main where

import Control.Concurrent
import Control.Exception as E

choose :: a -> a -> IO a
choose a b = do
   ready <- newMVar ()
   answer <- newEmptyMVar
   a_id <- forkIO (a `seq` takeMVar ready >> putMVar answer a)
   b_id <- forkIO (b `seq` takeMVar ready >> putMVar answer b)
   it <- takeMVar answer
   killThread a_id
   killThread b_id
   return it

main = do
   let big = sum [1..]
       small = sum [1..42]
   test1 <- choose big small
   test2 <- choose small big
   print (test1,test2)
