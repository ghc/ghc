module Main where

import GHC.RTS.Flags.Experimental (TraceFlags (..), getTraceFlags)

main :: IO ()
main = print . traceIpe =<< getTraceFlags
