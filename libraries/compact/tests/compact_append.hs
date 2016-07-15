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

main = do
  let val = ("hello", Just 42) :: (String, Maybe Int)
  str <- newCompact 4096 val

  let val2 = ("world", 42) :: (String, Int)
  str2 <- appendCompact str val2

  -- check that values where not corrupted
  assertEquals ("hello", Just 42) val
  assertEquals ("world", 42) val2
  -- check the values in the compact
  assertEquals ("hello", Just 42) (getCompact str)
  assertEquals ("world", 42) (getCompact str2)

  performMajorGC

  -- same checks again
  assertEquals ("hello", Just 42) val
  assertEquals ("world", 42) val2
  -- check the values in the compact
  assertEquals ("hello", Just 42) (getCompact str)
  assertEquals ("world", 42) (getCompact str2)
