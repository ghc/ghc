module Main where

import Foo
import System.Exit
import Control.Monad

main :: IO ()
main | fooTest [] = do
        replicateM 5 $ putStrLn "The quick brown fox jumps over the lazy dog"
        exitSuccess
     | otherwise = exitFailure
