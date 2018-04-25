module Main where

import System.IO.Error
import System.Process

main :: IO ()
main = do test1 `catchIOError` \e -> putStrLn ("Exc: " ++ show e)
          test2 `catchIOError` \e -> putStrLn ("Exc: " ++ show e)

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

