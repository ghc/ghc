module Test (test) where

import System.Environment

test :: IO ()
test = do
  print =<< getArgs
