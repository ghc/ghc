module Main where

import Control.Monad
import GHC.RTS.Flags

-- Ensure that +RTS -K0 is parsed
main :: IO ()
main = do
    flags <- getGCFlags
    unless (maxStkSize flags == 0) $ putStrLn "uh oh"
