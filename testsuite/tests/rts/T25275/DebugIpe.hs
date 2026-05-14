module Main where

import GHC.RTS.Flags.Experimental (DebugFlags (..), getDebugFlags)

main :: IO ()
main = print . ipe =<< getDebugFlags
