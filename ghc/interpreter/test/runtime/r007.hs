--!!! Testing Immutable Arrays (part 1)

import Array

a1 :: Array Int Int
a1 = array (1,10) [ (i,i*i) | i <- [1..10] ]


test1 = bounds a1
test2 = assocs a1
test3 = indices a1
test4 = elems a1

test5 = a1 // [(3,3),(4,4)]

-- note duplicate value and absent value
a1' :: Array Int Char
a1' = array (1,3) [(1,'a'), (1,'b'), (3,'c')]

test6 = a1' ! 1 -- duplicate array index
test7 = a1' ! 2 -- undefined array element
test8 = a1' ! 3 -- 'c'

test10 = a1 ! 0   -- should fail
test11 = a1 ! 11  -- should fail
test12 = [ a1 ! i | i <- [1..10] ]

