data One a = One a

choose (One x) = x
main = putStr (choose (One "hello world\n"))

