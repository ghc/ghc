module Main where

import GHC.Stack.Annotation.Experimental
import Control.Exception
import Control.Exception.Backtrace

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  annotateCallStackIO $ do
    annotateStackShowIO ([1..4] :: [Int]) $ do
      annotateStackStringIO "Lovely annotation" $ do
        throwIO $ ErrorCall "Backtrace Test"

