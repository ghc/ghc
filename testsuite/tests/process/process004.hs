module Main where

import System.IO.Error
import System.Process

main :: IO ()
main = do
  -- N.B. Only show the error type since the exact error text
  -- may depend upon precise system call which @process@ decided
  -- to use.
  let printError e = putStrLn ("Exc: " ++ show (ioeGetErrorType e))
  test1 `catchIOError` printError
  test2 `catchIOError` printError

test1 :: IO ()
test1 = do
  (_, _, _, commhand) <-
      runInteractiveProcess "true" [] (Just "/no/such/dir") Nothing
  exitCode <- waitForProcess commhand
  print exitCode

test2 :: IO ()
test2 = do
  commhand <- runProcess "true" [] (Just "/no/such/dir") Nothing
                Nothing Nothing Nothing
  exitCode <- waitForProcess commhand
  print exitCode

