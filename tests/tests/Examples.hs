module Examples where

-- | Fibonacci number of given 'Integer'.
--
-- Examples:
--
-- ghci> fib 5
-- 5
-- ghci> fib 10
-- 55
--
-- ghci> fib 10
-- 55
--
-- One more Example:
--
-- ghci> fib 5
-- 5
--
-- One more Example:
--
-- ghci> fib 5
-- 5
--
-- Example with an import:
--
-- ghci> import Data.Char
-- ghci> isSpace 'a'
-- False
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
