module Main where

import Graphics.Win32.Misc
import System.IO
import System.Win32.Types

testStdHandle :: Handle -> StdHandleId -> IO ()
testStdHandle haskHandle winStdHandle = do
  winHandle <- getStdHandle winStdHandle
  withHandleToHANDLE haskHandle $ print . (== winHandle)

main :: IO ()
main = do
  testStdHandle stdin  sTD_INPUT_HANDLE
  testStdHandle stdout sTD_OUTPUT_HANDLE
  testStdHandle stderr sTD_ERROR_HANDLE
