data List a = Cons a (List a)

hdL (Cons x y) = x
tlL (Cons x y) = y

test = Cons "hello world\n" test
main = putStr (hdL (tlL test))

