module Main where

import GHC.Conc
import Control.Exception

-- Create trivial invariants using a single TVar
main = do
  putStr "\nStarting\n"
  x <- atomically ( newTVar 42 )

  putStr "\nAdding trivially true invariant (no TVar access)\n"
  atomically ( alwaysSucceeds ( return 1 ) ) 

  putStr "\nAdding trivially true invariant (no TVar access)\n"
  atomically ( always ( return True ) ) 

  putStr "\nAdding a trivially true invariant (TVar access)\n"
  atomically ( alwaysSucceeds ( readTVar x ) ) 

  putStr "\nAdding an invraiant that's false when attemted to be added\n"
  Control.Exception.catch (atomically ( do writeTVar x 100
                                           alwaysSucceeds ( do v <- readTVar x 
                                                               if (v == 100) then throwDyn "URK" else return () )
                                           writeTVar x 0 ) )
      (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

  putStr "\nWriting to a TVar watched by a trivially true invariant\n"
  atomically ( writeTVar x 17 )

  putStr "\nAdding a second trivially true invariant (same TVar access)\n"
  atomically ( alwaysSucceeds ( readTVar x ) ) 

  putStr "\nWriting to a TVar watched by both trivially true invariants\n"
  atomically ( writeTVar x 18 )

  putStr "\nAdding a trivially false invariant (no TVar access)\n"
  Control.Exception.catch (atomically ( alwaysSucceeds ( throwDyn "Exn raised in invariant" ) ) )
      (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

  putStr "\nAdding a trivially false invariant (no TVar access)\n"
  Control.Exception.catch (atomically ( always ( throwDyn "Exn raised in invariant" ) ) )
      (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

  putStr "\nAdding a trivially false invariant (no TVar access)\n"
  Control.Exception.catch (atomically ( always ( return False ) ) )
      (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

  putStr "\nAdding a trivially false invariant (with TVar access)\n"
  Control.Exception.catch (atomically ( 
                alwaysSucceeds ( do t <- readTVar x
                                    throwDyn "Exn raised in invariant" ) ) )
      (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

  putStr "\nAdding a third invariant true if TVar != 42\n"
  atomically ( alwaysSucceeds ( do t <- readTVar x
                                   if (t == 42) then throwDyn "Exn raised in invariant" else return () ) )

  putStr "\nViolating third invariant by setting TVar to 42\n"
  Control.Exception.catch (atomically ( writeTVar x 42 ) )
      (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

  putStr "\nChecking final TVar contents\n"
  t <- atomically ( readTVar x )
  putStr ("Final value = " ++ (show t) ++ "\n")

  putStr "\nDone\n"
         
