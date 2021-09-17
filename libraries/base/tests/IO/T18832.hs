{-# Language ScopedTypeVariables #-}
module Main where

import Prelude
import System.Directory
import System.FilePath
import System.IO
import Control.Monad (forM_, forever, when)
import Control.Exception
import Control.Concurrent
--import Data.Time

-- How many `openHandle` calls in the test
-- On a laptop:
-- * when set to 1k, it ocasionally reproduces the failure
-- * when set to 10k, it ocasionally fails to reproduce
n :: Int
n = 10000

main :: IO ()
main = test "."

test :: FilePath -> IO ()
test dir' = do
  let dir = dir' </> "repro"
  createDirectoryIfMissing True dir
  availableNames <- newChan :: IO (Chan FilePath)
  writeList2Chan availableNames [ dir </> "repro" ++ show (i :: Int) | i <- [1..30]]
  toClose <- newChan :: IO (Chan (Handle, FilePath))
  maybeDelete <- newChan :: IO (Chan FilePath)
  deleter <- forkIO (getChanContents maybeDelete >>= mapM_ (recycle availableNames))
  closer <- forkIO (getChanContents toClose >>= mapM_ (keepClosing availableNames))
  resultMVar <- newEmptyMVar
  openingThread <- keepOpening availableNames toClose maybeDelete `forkFinally`
                      putMVar resultMVar
  interrupter <- forkIO $ forever $ do
    threadDelay (10^3)
    throwTo openingThread Interrupt

  result <- readMVar resultMVar

  -- cleanup
  mapM_ killThread [interrupter, deleter, closer]
  removeDirectoryRecursive dir

  either throwIO (const $ putStrLn "No failures observed - success") result


keepOpening :: Chan FilePath -> Chan (Handle, FilePath) -> Chan FilePath -> IO ()
keepOpening availableNames toClose maybeDelete =
  uninterruptibleMask $ \ restore -> do
    filepaths <- take n <$> getChanContents availableNames
    forM_ filepaths $ \filepath -> do
      --now <- getCurrentTime
      h <- (Just <$> restore (openFile filepath WriteMode)) `catch` \(_ :: Interrupt) -> do
            writeChan maybeDelete filepath
            pure Nothing
      --elapsed <- (`diffUTCTime` now) <$> getCurrentTime
      --print elapsed
      case h of
        Nothing -> pure ()
        Just h -> writeChan toClose (h, filepath)

data Interrupt = Interrupt deriving (Show)
instance Exception Interrupt

recycle :: Chan FilePath -> FilePath -> IO ()
recycle availableNames name = do
  exist <- doesFileExist name
  when exist $ removeFile name
  writeChan availableNames name

keepClosing :: Chan FilePath -> (Handle, FilePath) -> IO ()
keepClosing availableNames (handle, name) = do
  hClose handle
  removeFile name
  writeChan availableNames name
