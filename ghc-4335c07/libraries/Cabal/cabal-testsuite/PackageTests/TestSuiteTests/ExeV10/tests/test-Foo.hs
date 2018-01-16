module Main where

import Foo
import System.Exit
import Control.Monad

main :: IO ()
main | fooTest [] = do
        -- Make sure that the output buffer is drained
        replicateM 10000 $ putStrLn "The quick brown fox jumps over the lazy dog"
        exitSuccess
     | otherwise = exitFailure
