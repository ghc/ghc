data Inf a = A (Inf a)

hd (A x) = x

choose (A (A x)) =  "hello world\n"
mk f = f (mk f)
main = putStr (choose (hd (mk A)))

