-- | This should not issue an @-Wunused-packages@ warning for @system-cxx-std-lib@.
module Main where

main :: IO ()
main = putStrLn "hello world"
