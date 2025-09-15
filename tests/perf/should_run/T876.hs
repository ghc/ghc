-- This test allocates a lot more if length is
-- not a good consumer

module Main where
import System.Environment (getArgs)

foo :: Int -> Int
foo n = sum [ length [i..n] | i <- [1..n] ]

main = do { [arg] <- getArgs
          ; print (foo (read arg)) }
