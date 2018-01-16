-- test for threadstatus, checking (mvar read, mvar block reasons)
-- created together with fixing GHC ticket #9333

module Main where

import Control.Concurrent
import GHC.Conc
import GHC.Conc.Sync

main = do
  -- create MVars to block on
  v1 <- newMVar "full"
  v2 <- newEmptyMVar
  -- create a thread which fills both MVars
  parent <- myThreadId
  putStrLn "p: forking child thread"
  child <- forkIO $
           do putStrLn "c: filling full MVar" -- should block
              putMVar v1 "filled full var"
              yield
              putStrLn "c: filling empty MVar (expect parent to be blocked)"
              stat2 <- threadStatus parent
              putStrLn ("c: parent is " ++ show stat2)
              putMVar v2 "filled empty var"
  yield
  putStrLn "p: emptying full MVar (expect child to be blocked on it)"
  stat1 <- threadStatus child
  putStrLn ("p: child is " ++ show stat1)
  s1 <- takeMVar v1 -- should unblock child
  putStrLn ("p: from MVar: " ++ s1)
  putStrLn "p: reading empty MVar"
  s2 <- readMVar v2 -- should block
  putStrLn ("p: from MVar: " ++ s2)
