data List a = Cons a (List a)

hdL (Cons x y) = x
tlL (Cons x y) = y

mk f x = f x (mk f x)
main = putStr (hdL (tlL (mk Cons "hello world!\n")))

