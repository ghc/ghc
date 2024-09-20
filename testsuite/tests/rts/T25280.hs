module Main where

import Control.Concurrent
import qualified System.Posix.Process as SPP
import System.Directory
import Control.Monad
import System.Exit

main = do
  fds <- listDirectory "/proc/self/fd"
  go 0 5 fds

go :: Int -> Int -> [FilePath] -> IO ()
go i n fds
  | i == n = return ()
  | otherwise = do
    fds' <- listDirectory "/proc/self/fd"
    when (fds /= fds') $ do
      putStrLn "File descriptors changed after fork:"
      putStrLn $ "Before:" ++ show fds
      putStrLn $ "After: " ++ show fds'
      exitFailure
    pid <- SPP.forkProcess $ go (i+1) n fds
    void (SPP.getProcessStatus True True pid)
