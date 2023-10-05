x = 1 / 2 / 2
a = {-# SCC ann #-} 1 / 2 / 2
b = 1 / 2 / {-# SCC ann #-} 2

main = print (x, a == x, b == x)
