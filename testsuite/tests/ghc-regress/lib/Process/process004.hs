module Main where

import System.Process

main = do
  (_, _, _, commhand) <-
      runInteractiveProcess "true" [] (Just "/no/such/dir") Nothing
  exitCode <- waitForProcess commhand
  print exitCode

  commhand <- runProcess "true" [] (Just "/no/such/dir") Nothing
                Nothing Nothing Nothing
  exitCode <- waitForProcess commhand
  print exitCode
