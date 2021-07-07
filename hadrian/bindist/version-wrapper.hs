{-# LANGUAGE CPP #-}
module Main (main) where

import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitWith)
import System.Process (spawnProcess, waitForProcess)
import System.FilePath (replaceFileName)

exe = EXE_PATH

main :: IO ()
main = do
  args <- getArgs
  exe_name <- getExecutablePath
  ph <- spawnProcess (replaceFileName exe_name exe) args
  code <- waitForProcess ph
  exitWith code
