import Data.Word
fib :: Word -> Word
fib 0 = 1
fib 1 = 1
fib n = l + r 
  where l = fib (n-2); r = fib (n-1)

main = print (fib 20)
