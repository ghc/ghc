--!!! Dictionary bug demo 
import Array

a :: Array Int Int
a = array (1,10) [ (i,i*i) | i <- [1..10] ]

test1 = show a
test2 = show a

test3 = let a = array (1,10) [ (i,i*i) | i <- [1..10] ] in show a
test4 = let a = array (1,10) [ (i,i*i) | i <- [1..10] ] in show a