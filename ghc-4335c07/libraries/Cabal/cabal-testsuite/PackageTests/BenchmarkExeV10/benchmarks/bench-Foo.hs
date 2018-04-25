module Main where

import Foo
import System.Exit

main :: IO ()
main | fooTest [] = exitSuccess
     | otherwise = exitFailure
