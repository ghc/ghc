module Examples where

-- | Fibonacci number of given 'Integer'.
--
-- Examples:
--
-- >>> fib 5
-- 5
-- >>> fib 10
-- 55
--
-- >>> fib 10
-- 55
--
-- One more Example:
--
-- >>> fib 5
-- 5
--
-- One more Example:
--
-- >>> fib 5
-- 5
--
-- Example with an import:
--
-- >>> import Data.Char
-- >>> isSpace 'a'
-- False
--
-- >>> putStrLn "foo\n\nbar"
-- foo
-- <BLANKLINE>
-- bar
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
