{-# LANGUAGE BangPatterns #-}
main = print (f 20 30)

{-# NOINLINE f #-}
f x = let !x' = fib x in \y -> x' + fib y

fib :: Int -> Int
fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)
