module Main where

import GHC.Conc.Sync

main :: IO ()
main = listThreads >>= print
