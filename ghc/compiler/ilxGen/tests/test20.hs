
data N = Z | S N

res Z x y = (# x, y #)
res (S n) x y = res n x y

(# x, y #) = res (S Z) "no!" "hello world\n"

main = putStr y
