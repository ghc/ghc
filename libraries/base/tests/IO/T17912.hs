{-# Language ScopedTypeVariables #-}
module Main where

import System.IO
import GHC.IO.Handle.FD
import Control.Exception
import Control.Concurrent
import System.Exit
import Data.Either
import System.Process

main = do
  ecode <- waitForProcess =<< spawnProcess "mkfifo" ["fifo"]
  case ecode of
    ExitFailure code -> putStrLn "mkfifo failed" -- assert is not linux?
    ExitSuccess -> do
      passed <- newEmptyMVar
      opener <- forkIO $ do
        (openFileBlocking "fifo" WriteMode >> return ())
          `catch` \(e:: AsyncException) -> do
             if e == ThreadKilled then do
                putStrLn "openFileBlocking successfully interrupted"
                putMVar passed True
             else print e
             throwIO e
      threadDelay 10
      forkIO $ killThread opener
      forkIO $ do
        threadDelay (10^5)
        putStrLn "timeout!"
        putMVar passed False
      res <- readMVar passed
      case res of
        True -> exitSuccess
        False -> exitFailure


