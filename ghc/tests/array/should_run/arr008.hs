-- !!! Array - out-of-range (index,value) pairs
-- 
-- supplying a list containing one or more pairs 
-- with out-of-range index is undefined.
--
--
import Array

main = 
 let 
  a1 = array (1,0) []
  a2 = array (0,1) (zip [0..] ['a'..'z'])
 in
 print (a1::Array Int Int) >> print a2
