module Main where

import Control.Exception
import System.Mem

import Data.Compact

assertFail :: String -> IO ()
assertFail msg = throwIO $ AssertionFailed msg

assertEquals :: (Eq a, Show a) => a -> a -> IO ()
assertEquals expected actual =
  if expected == actual then return ()
  else assertFail $ "expected " ++ (show expected)
       ++ ", got " ++ (show actual)

-- test :: (Word -> a -> IO (Maybe (Compact a))) -> IO ()
test func = do
  let val = ("hello", 1, 42, 42, Just 42) ::
        (String, Int, Int, Integer, Maybe Int)
  str <- func 4096 val

  -- check that val is still good
  assertEquals ("hello", 1, 42, 42, Just 42) val
  -- check the value in the compact
  assertEquals ("hello", 1, 42, 42, Just 42) (getCompact str)
  performMajorGC
  -- check again val
  assertEquals ("hello", 1, 42, 42, Just 42) val
  -- check again the value in the compact
  assertEquals ("hello", 1, 42, 42, Just 42) (getCompact str)

main = do
  test newCompact
  test newCompactNoShare
