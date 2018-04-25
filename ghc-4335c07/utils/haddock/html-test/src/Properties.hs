module Properties where

-- | Fibonacci number of given 'Integer'.
--
-- prop> fib n <= fib (n + 1)
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
