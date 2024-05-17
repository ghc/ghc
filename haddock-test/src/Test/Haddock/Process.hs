{-# LANGUAGE RecordWildCards #-}

module Test.Haddock.Process where

import Control.Monad

import System.Exit
import System.IO
import System.Process

type Environment = [(String, String)]

data ProcessConfig = ProcessConfig
  { pcArgs :: [String]
  , pcWorkDir :: Maybe FilePath
  , pcEnv :: Maybe Environment
  , pcStdIn :: Maybe Handle
  , pcStdOut :: Maybe Handle
  , pcStdErr :: Maybe Handle
  }
  deriving (Show)

processConfig :: ProcessConfig
processConfig =
  ProcessConfig
    { pcArgs = []
    , pcWorkDir = Nothing
    , pcEnv = Nothing
    , pcStdIn = Nothing
    , pcStdOut = Nothing
    , pcStdErr = Nothing
    }

runProcess' :: FilePath -> ProcessConfig -> IO ProcessHandle
runProcess' path (ProcessConfig{..}) =
  runProcess
    path
    pcArgs
    pcWorkDir
    pcEnv
    pcStdIn
    pcStdOut
    pcStdErr

-- | Wait for a process to finish running. If it ends up failing, print out the
-- error message.
waitForSuccess :: String -> Handle -> ProcessHandle -> IO Bool
waitForSuccess msg out handle = do
  succeeded <- fmap (== ExitSuccess) $ waitForProcess handle
  unless succeeded $ hPutStrLn out msg
  pure succeeded
