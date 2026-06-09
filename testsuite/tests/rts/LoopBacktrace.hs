{-# OPTIONS_GHC -finfo-table-map -forig-thunk-info #-}

import GHC.Exception.Backtrace.Experimental

x :: Integer
x = x + 1

testing :: IO ()
testing = do
  putStrLn "hello"
  print x
  putStrLn "world"

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  testing
