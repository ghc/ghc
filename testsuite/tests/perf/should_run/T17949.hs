module Main where

import Debug.Trace

main :: IO ()
main = do
  traceEventIO (show [0..1234567])
