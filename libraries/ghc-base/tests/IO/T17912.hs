{-# Language ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Exception
import System.IO
import System.Exit
import System.Process
import GHC.IO.Handle.FD

main = do
  ecode <- waitForProcess =<< spawnProcess "mkfifo" ["fifo"]
  case ecode of
    ExitFailure code -> putStrLn "mkfifo failed"
    ExitSuccess -> do
      passed <- newEmptyMVar
      opener <- forkIO $
        (openFileBlocking "fifo" WriteMode >> return ())
          `catch` \(e:: AsyncException) -> do
             if e == ThreadKilled then do
                putStrLn "openFileBlocking successfully interrupted"
                putMVar passed True
             else print e
             throwIO e
      threadDelay 1000
      forkIO $ killThread opener
      forkIO $ do
        threadDelay (10^6)
        putStrLn "timeout!"
        putMVar passed False
      res <- readMVar passed
      case res of
        True -> exitSuccess
        False -> exitFailure
