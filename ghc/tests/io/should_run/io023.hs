-- !!! Testing output on stdout
module Main(main) where

-- stdout is buffered, so test if its buffer
-- is flushed upon program termination.
main :: IO ()
main = putStr "Hello"
