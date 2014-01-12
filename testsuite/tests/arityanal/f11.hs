module F11 where

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

f11f = \z -> let x = fib 1000
             in \y -> x+y

f11 = (f11f 5 6, f11f 7 8)
