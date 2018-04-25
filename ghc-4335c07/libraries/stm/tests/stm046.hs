{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC.Conc
import Control.Concurrent
import Control.Exception

inittvars :: STM (TVar String, TVar String)
inittvars = do v1 <- newTVar "Hello "
               v2 <- newTVar "world\n"
               return (v1, v2)

stmops :: TVar String -> TVar String -> STM String
stmops v1 v2 = do s1 <- readTVar v1
                  s2 <- readTVar v2
                  return (s1 ++ s2)

stmupdates :: TVar String -> TVar String -> STM ()
stmupdates v1 v2 = do writeTVar v1 "About to throw exception"
                      throw (ErrorCall "Exn holding string")

internalexn :: TVar String -> TVar String -> STM ()
internalexn v1 v2 = catchSTM ( do writeTVar v1 "About to throw exception"
                                  throw (ErrorCall "Exn holding string" ))
                             (\e -> let _ = e :: SomeException in
                                    writeTVar v1 "Reached handler ")

internalexn2 :: TVar String -> TVar String -> STM ()
internalexn2 v1 v2 = catchSTM ( do writeTVar v1 "Hello " )
                              (\e -> let _ = e :: SomeException in
                                     writeTVar v1 "Reached handler2 ")

-- Exception handling within / around memory transactions
main = do putStr "Before\n"
          (sv1, sv2) <- atomically ( inittvars )

          putStr "Reading from svars:            "
          x <- atomically ( stmops sv1 sv2 )
          putStr x

          putStr "Abandoning update with exception\n"
          Control.Exception.catch (atomically ( stmupdates sv1 sv2 ))
                     (\(e::ErrorCall) -> putStr "Abandoned\n")

          putStr "Reading from svars:            "
          x <- atomically ( stmops sv1 sv2 )
          putStr x

          putStr "Atomic block with internal exception\n"
          atomically ( internalexn sv1 sv2 )

          putStr "Reading from svars:            "
          x <- atomically ( stmops sv1 sv2 )
          putStr x

          putStr "Atomic block with handler but no exception\n"
          atomically ( internalexn2 sv1 sv2 )

          putStr "Reading from svars:            "
          x <- atomically ( stmops sv1 sv2 )
          putStr x

          return ()
