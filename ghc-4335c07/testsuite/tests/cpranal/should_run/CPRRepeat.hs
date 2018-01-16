repeat' :: x -> [x]
repeat' x = x : repeat' x

main = print (repeat' () !! 10 )
