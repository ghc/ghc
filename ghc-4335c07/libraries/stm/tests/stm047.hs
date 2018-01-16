{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC.Conc
import Control.Exception
import Foreign.StablePtr
import System.IO

inittvar :: STM (TVar String)
inittvar = newTVar "Hello world"

deadlock0 :: STM String
deadlock0 = retry

deadlock1 :: TVar String -> STM String
deadlock1 v1 = do s1 <- readTVar v1
                  retry

-- Basic single-threaded operations with retry
main = do newStablePtr stdout
          putStr "Before\n"
          t1 <- atomically ( newTVar 0 )

          -- Atomic block that contains a retry but does not perform it
          r <- atomically ( do r1 <- readTVar t1
                               if (r1 /= 0) then retry else return ()
                               return r1 )
          putStr ("Survived unused retry\n")

          -- Atomic block that retries after reading 0 TVars
          s1 <- Control.Exception.catch (atomically retry )
                   (\(e::SomeException) -> return ("Caught: " ++ (show e) ++ "\n"))
          putStr s1

          -- Atomic block that retries after reading 1 TVar
          t1 <- atomically ( inittvar )
          s1 <- Control.Exception.catch (atomically ( deadlock1 t1 ))
                   (\(e::SomeException) -> return ("Caught: " ++ (show e) ++ "\n"))
          putStr s1


          return ()
