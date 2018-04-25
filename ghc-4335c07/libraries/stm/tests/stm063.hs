{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC.Conc
import Control.Exception
import Foreign.StablePtr
import System.IO
import Control.Concurrent.MVar

-- Test invariants using updates & blocking in invariants
main = do
  m <- newEmptyMVar
  forkIO (do_test m)
  -- We do the test in a separate thread, because this test relies on
  -- being able to catch BlockedIndefinitely, and the main thread
  -- won't receive that exception under GHCi because it is held alive
  -- by the interrupt (^C) handler thread.
  newStablePtr m
  -- the MVar m must be kept alive, otherwise when the subthread is
  -- BlockedIndefinitely, the MVar will be unreachable and the main
  -- thread will also be considered to be BlockedIndefinitely.
  takeMVar m

do_test m = do
  newStablePtr stdout

  putStr "\nStarting\n"
  (x1, x2, x3) <- atomically ( do x1 <- newTVar 0
                                  x2 <- newTVar 0 
                                  x3 <- newTVar 0 
                                  return (x1, x2, x3))

  putStr "\nAttaching successful invariant that makes an update\n";
  atomically ( alwaysSucceeds ( writeTVar x1 42 ) ) 

  putStr "\nAttaching successful invariant that uses retry&orelse internally\n";
  atomically ( alwaysSucceeds ( retry `orElse` return () ) ) 

  putStr "\nAttaching a failed invariant that makes an update\n";
  Control.Exception.catch (atomically ( do writeTVar x1 17
                                           alwaysSucceeds ( throw (ErrorCall "Exn raised in invariant") ) ) )
      (\(e::SomeException) -> putStr ("Caught: " ++ (show e) ++ "\n"))

  putStr "\nAttaching an invariant that blocks\n";
  forkIO ( do threadDelay 1000000
              atomically ( writeTVar x1 10 ) 
              return ()) 
  atomically ( do alwaysSucceeds ( do v1 <- readTVar x1
                                      if (v1 == 0) then retry else return () )
              )
  
  putStr "\nAnother update to the TVar with the blocking invariant\n"
  atomically ( writeTVar x1 20 ) 

  putStr "\nUpdate the TVar to cause the invariant to block again (expect thread blocked indef)\n"
  Control.Exception.catch (atomically ( writeTVar x1 0 ))
                 (\(e::SomeException) -> putStr ("Caught: " ++ (show e) ++ "\n"))

  putMVar m ()         
