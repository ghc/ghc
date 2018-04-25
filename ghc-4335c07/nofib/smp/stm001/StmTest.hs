module Main where

import Control.Concurrent
import Control.Concurrent.STM
import System.IO

data State = State {
  vt :: TVar Int,
  vm :: MVar Int,
  chan :: TChan (),
  count :: TVar Int
  }

loopmax = 100000
numthreads = 50

main
  = do t <- atomically (newTVar 0)
       m <- newEmptyMVar
       putMVar m 0
       c <- atomically (newTChan)
       cnt <- atomically (newTVar 0)
       let st = State t m c cnt
       forkIter numthreads (proc st domv loopmax)
       atomically (readTChan c)
       return ()

proc :: State -> (State -> IO ()) -> Int -> IO ()
proc st w 0 = do c <- atomically (do cnt <- readTVar (count st)
                                     writeTVar (count st) (cnt+1)
                                     if cnt+1 >= numthreads
                                        then writeTChan (chan st) ()
                                        else return ()
                                     return cnt)
                 return ()
proc st w i
  = do w st
       proc st w (i-1)

dotv :: State -> IO ()
dotv st
  = do n <- atomically (do n <- readTVar (vt st)
                           writeTVar (vt st) (n+1)
                           return n)
       return ()

domv :: State -> IO ()
domv st
  = do n <- takeMVar (vm st)
       putMVar (vm st) (n+1)
       return ()

forkIter :: Int -> IO () -> IO ()
forkIter n p
  = iter n (do forkIO p
               return ())

iter :: Int -> IO () -> IO ()
iter 0 _ = return ()
iter n f
  = do f
       iter (n-1) f
