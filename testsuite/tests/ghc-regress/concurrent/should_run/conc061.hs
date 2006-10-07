module Main where

import GHC.Conc
import Control.Concurrent
import Control.Exception

main = do putStr "Starting\n";
          t <- atomically (newTVar 42)

          v <- atomically (readTVar t)
          putStr ("TVar contains " ++ (show v) ++ "\n")

          -- ......................................................................
          -- Check that we roll back when an exception leaves an atomic block

          putStr ("Raising uncaught exn in atomic block\n");
          Control.Exception.catch (atomically ( 
                                     do writeTVar t 17
                                        throwDyn "Exn raised in a tx" ) )
           (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

          v <- atomically (readTVar t)
          putStr ("TVar contains " ++ (show v) ++ "\n")

          -- ......................................................................
          -- Check that we commit a catchSTM nested tx

          putStr ("Trying a catchSTM without raising an exception\n");
          Control.Exception.catch (atomically ( 
                                     catchSTM ( do writeTVar t 17 )
                                              ( \e -> throw e  ) ) )
           (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

          v <- atomically (readTVar t)
          putStr ("TVar contains " ++ (show v) ++ "\n")

          -- ......................................................................
          -- Check that we roll back when an exception is caught and rethrown in
          -- an atomic block

          putStr ("Raising caught and rethrown exn in atomic block\n");
          Control.Exception.catch (atomically ( 
                                     catchSTM ( do writeTVar t 42
                                                   throwDyn "Exn raised in a tx" )
                                              ( \e -> throw e  ) ) )
           (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

          v <- atomically (readTVar t)
          putStr ("TVar contains " ++ (show v) ++ "\n")

          -- ......................................................................
          -- Check that we roll back just the "catchSTM" block when an exception is
          -- raised in it (but caught later in the same atomic block)

          putStr ("Raising caught and rethrown exn in atomic block\n");
          v <- atomically ( 
                    do writeTVar t 0
                       catchSTM ( do writeTVar t 1
                                     throwDyn "Exn raised in a tx" )
                                ( \_ -> return () ) 
                       readTVar t )
          putStr ("TVar contained " ++ (show v) ++ " at end of atomic block\n")

          v <- atomically (readTVar t)
          putStr ("TVar contains " ++ (show v) ++ "\n")

          -- ......................................................................
          -- Check that 'retry' can propagate through a catchSTM

          putStr ("Testing retry inside catchSTM\n");
          Control.Exception.catch (atomically ( 
                                     ( catchSTM ( retry )
                                                ( \e -> throw e  ) ) 
                                     `orElse` ( return () ) ) )
           (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))

          v <- atomically (readTVar t)
          putStr ("TVar contains " ++ (show v) ++ "\n")

