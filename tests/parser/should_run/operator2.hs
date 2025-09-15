
(|..) :: a -> a -> a
x |.. y = y

f = [2|..3]

main = putStrLn (show f)
