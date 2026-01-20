module Main where

import GHC.Stack.Annotation.Experimental
import Control.Exception
import Control.Exception.Backtrace

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  print $ foo 500

foo :: Int -> Int
foo n =
  annotateCallStack $
    annotateStackShow ([1..4] :: [Int]) $
      annotateStackString "Lovely annotation" $
        throw $ ErrorCall $ "Backtrace Test: " ++ show (n * n * n)

