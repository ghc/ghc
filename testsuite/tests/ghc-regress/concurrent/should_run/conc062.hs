module Main where

import GHC.Conc
import Control.Exception

-- Test invariants using multiple TVars
main = do
  putStr "\nStarting\n"
  (x1, x2, x3) <- atomically ( do x1 <- newTVar 0
                                  x2 <- newTVar 0 
                                  x3 <- newTVar 0 
                                  return (x1, x2, x3))

  putStr "\nAttaching invariant\n";
  atomically ( alwaysSucceeds ( do v1 <- readTVar x1
                                   v23 <- readTVar (if (v1 >= 0) then x2 else x3)
                                   if (v23 > v1) then throwDyn "Exn" else return () ) )

  putStr "\nTouching invariant (should keep on same TVars)\n"
  atomically ( do writeTVar x1 1
                  writeTVar x2 1 )

  putStr "\nTouching invariant (should move it to other TVars)\n"
  atomically ( do writeTVar x1 (-1)
                  writeTVar x3 (-1) )

  putStr "\nTouching invariant (should keep on same TVars)\n"
  atomically ( do writeTVar x1 (-2)
                  writeTVar x3 (-3) )

  putStr "\nChecking TVar contents\n"
  (t1, t2, t3) <- atomically ( do t1 <- readTVar x1
                                  t2 <- readTVar x2
                                  t3 <- readTVar x3
                                  return (t1, t2, t3))
  putStr ("Contents = (" ++ (show t1) ++ "," ++ (show t2) ++ "," ++ (show t3) ++ ")\n")

  putStr "\nDone\n"
         
