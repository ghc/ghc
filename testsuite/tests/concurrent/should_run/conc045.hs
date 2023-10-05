module Main where

import GHC.Conc
import Control.Concurrent

snapshot t1 t2 = atomically ( do v1 <- readTVar t1
                                 v2 <- readTVar t2
                                 return (v1, v2) )

twiddle mv _ _ 0 = putMVar mv ()
twiddle mv t1 t2 n = do atomically ( do v1 <- readTVar t1
                                        v2 <- readTVar t2
                                        writeTVar t2 (v1+1)
                                        writeTVar t1 (v2+1) )
                        twiddle mv t1 t2 (n-1)
                  

-- Contended updates to a pair of TVars 
main = do
  putStr "Before\n"
  (t1,t2) <- atomically ( do t1 <- newTVar 0
                             t2 <- newTVar 1
                             return (t1, t2))

  -- MVars used to signal completion 
  t1c <- newEmptyMVar
  t2c <- newEmptyMVar

  forkIO (twiddle t1c t1 t2 1000)
  forkIO (twiddle t2c t1 t2 1000)

  -- Wait for threads to exit
  takeMVar t1c
  takeMVar t2c

  -- Display final state
  (r1,r2) <- snapshot t1 t2 
  putStr ("After " ++ (show r1) ++ " , " ++ (show r2) ++ "\n")
         
