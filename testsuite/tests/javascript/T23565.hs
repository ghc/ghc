module Main where

import GHC.JS.Prim
import Control.Exception
import System.Exit

main :: IO ()
main = foo `catch` \(JSException val s) -> do
  -- check that the message (including call stack) hasn't too many lines
  -- (#23565)
  if length (lines s) >= 10
    then putStrLn "Failure: too many lines" >> exitFailure
    else pure ()


foreign import javascript "foo"
  foo :: IO ()
