module Main where
fib :: Int -> Int
fib 0 = 1 -- GHC should merge the blocks
fib 1 = 1 -- of these two alternatives
fib n = fib (n-1) + fib (n-2)
main = print $ fib 10
