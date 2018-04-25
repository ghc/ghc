-- | Test-suite framework and utility functions.
module TestUtils (
    simpleMatch
  ) where

import Control.Monad
import System.Exit

simpleMatch :: String -> String -> String -> IO ()
simpleMatch test expected actual =
  when (actual /= expected) $ do
    putStrLn $ "Test `" ++ test ++ "' failed!"
    putStrLn "-----------------------------"
    putStrLn $ "Expected: " ++ expected
    putStrLn "-----------------------------"
    putStrLn $ "Actual: " ++ actual
    putStrLn "-----------------------------"
    exitFailure

