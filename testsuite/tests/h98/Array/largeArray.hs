import Array

main = print (((!1).inc.inc.inc.inc.inc.inc.inc.inc.inc.inc) a)

size = 60

a :: Array Int Integer
a = listArray (1,size) [1..]

inc a = accum (+) a [(i,1) | i <- [1..size]]
