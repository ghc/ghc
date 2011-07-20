
-- Test for trac #414
-- If main is not exported from Main then we should emit an error
-- instead of running it anyway

module Main () where

main = putStrLn "Hello, World"

