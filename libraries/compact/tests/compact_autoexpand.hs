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
  -- create a compact large 4096 bytes (minus the size of header)
  -- add a value that is 1024 cons cells, pointing to 7 INTLIKE
  -- each cons cell is 1 word header, 1 word data, 1 word next
  -- so total 3072 words, 12288 bytes on x86, 24576 on x86_64
  -- it should not fit in one block
  let val = replicate 4096 7 :: [Int]
  str <- newCompact 1 val
  assertEquals val (getCompact str)
  performMajorGC
  assertEquals val (getCompact str)
