-- This example is from the User's Guide, so we better make sure it
-- produces the correct output.

main = print (f 30 + g 30)
  where
    f n  = fib n
    g n  = fib (n `div` 2)

fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)
