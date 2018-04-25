module Main where

import Control.Concurrent
import Control.Concurrent.STM
import System.IO

data StateM = StateM {
  i :: Int,
  s :: String,
  b :: Bool
  }

data State = State {
  ti :: TVar Int,
  ts :: TVar String,
  tb :: TVar Bool,

  mv :: MVar StateM,

  chan :: TChan (),
  count :: TVar Int
  }

loopmax = 100000
numthreads = 10

main
  = do i <- atomically (newTVar 0)
       s <- atomically (newTVar "1")
       b <- atomically (newTVar False)
       m <- newEmptyMVar
       let sm = StateM 0 "1" False
       putMVar m sm
       c <- atomically (newTChan)
       cnt <- atomically (newTVar 0)
       let st = State i s b m c cnt
{-
       forkIter numthreads (proc st tvir loopmax)
       forkIter numthreads (proc st tvirw loopmax)
       forkIter numthreads (proc st tvsr loopmax)
       forkIter numthreads (proc st tvsrw loopmax)
       forkIter numthreads (proc st tvbr loopmax)
       forkIter numthreads (proc st tvbrw loopmax)
-}
       forkIter numthreads (proc st mvir loopmax)
       forkIter numthreads (proc st mvirw loopmax)
       forkIter numthreads (proc st mvsr loopmax)
       forkIter numthreads (proc st mvsrw loopmax)
       forkIter numthreads (proc st mvbr loopmax)
       forkIter numthreads (proc st mvbrw loopmax)

       atomically (readTChan c)
       return ()

proc :: State -> (State -> IO ()) -> Int -> IO ()
proc st w 0 = do c <- atomically (do cnt <- readTVar (count st)
                                     writeTVar (count st) (cnt+1)
                                     if cnt+1 >= (numthreads*6)
                                        then writeTChan (chan st) ()
                                        else return ()
                                     return cnt)
                 return ()
proc st w i
  = do w st
       proc st w (i-1)

tvir :: State -> IO ()
tvir st
  = do n <- atomically (readTVar (ti st))
       return ()

tvirw :: State -> IO ()
tvirw st
  = do n <- atomically (do n <- readTVar (ti st)
                           writeTVar (ti st) (n+1)
                           return n)
       return ()

tvsr :: State -> IO ()
tvsr st
  = do s <- atomically (readTVar (ts st))
       return ()

tvsrw :: State -> IO ()
tvsrw st
  = do s <- atomically (do s <- readTVar (ts st)
                           writeTVar (ts st) (randomString s)
                           return s)
       return ()

tvbr :: State -> IO ()
tvbr st
  = do b <- atomically (readTVar (tb st))
       return ()

tvbrw :: State -> IO ()
tvbrw st
  = do b <- atomically (do b <- readTVar (tb st)
                           writeTVar (tb st) (not b)
                           return b)
       return ()

mvir :: State -> IO ()
mvir st
  = do m <- takeMVar (mv st)
       let i2 = (i m)
       putMVar (mv st) m
       return ()

mvirw :: State -> IO ()
mvirw st
  = do m <- takeMVar (mv st)
       let i2 = (i m)
           m2 = StateM (i2+1) (s m) (b m)
       putMVar (mv st) m2
       return ()

mvsr :: State -> IO ()
mvsr st
  = do m <- takeMVar (mv st)
       let s2 = (s m)
       putMVar (mv st) m
       return ()

mvsrw :: State -> IO ()
mvsrw st
  = do m <- takeMVar (mv st)
       let s2 = (s m)
           m2 = StateM (i m) (randomString s2) (b m)
       putMVar (mv st) m2
       return ()

mvbr :: State -> IO ()
mvbr st
  = do m <- takeMVar (mv st)
       let b2 = (b m)
       putMVar (mv st) m
       return ()

mvbrw :: State -> IO ()
mvbrw st
  = do m <- takeMVar (mv st)
       let b2 = (b m)
           m2 = StateM (i m) (s m) (not b2)
       putMVar (mv st) m2
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

randomString :: String -> String
randomString str
  = case str of 
      "1" -> "2"
      "2" -> "3"
      "3" -> "1"
