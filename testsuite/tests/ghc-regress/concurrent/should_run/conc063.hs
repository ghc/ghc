module Main where

import GHC.Conc
import Control.Exception
import IO
import Foreign.StablePtr
import System.IO

-- Test invariants using updates & blocking in invariants
main = do
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
                                           alwaysSucceeds ( throwDyn "Exn raised in invariant" ) ) )
      (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

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
                 (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

         
