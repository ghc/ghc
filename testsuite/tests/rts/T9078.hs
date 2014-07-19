module Main where

import Control.Monad
import System.Mem.StableName

main :: IO ()
main = replicateM_ 500000 (makeStableName foo)

foo :: Int
foo = 1
