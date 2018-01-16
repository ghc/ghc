module Main where

import Control.Concurrent
import System.IO
import System.Process
import Control.Monad
import Control.Exception

main :: IO ()
main = do 
  print =<< readProcess "cat" [] "yan\ntan\tether\n"
  print =<< readProcessWithExitCode "cat" [] "yan\ntan\tether\n"
  print =<< readProcessWithExitCode "sh" ["-c", "echo stdout; echo stderr 1>&2; exit 3"] ""
  e <- (try $ readProcess "sh" ["-c", "echo stdout; echo stderr 1>&2; exit 3"] "")
  print (e :: Either SomeException String)
