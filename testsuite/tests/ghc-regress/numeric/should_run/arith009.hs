-- a prefix minus precedence test

f :: Int -> Int -> Int -> Int
f x y z = - x * y ^ z

main = putStr (shows (f 5 2 3) "\n")
