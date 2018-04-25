-- !!! Array - derived ops
-- 
-- testing the well-behavedness of 
-- derived ops for empty and non-empty arrays
--
import Data.Array

main = 
 let 
  a1 = array (1,0) ([]::[(Int,Int)])
  a2 = array (1,26) (zip [1..] ['a'..'z'])

  dump a = (bounds a, indices a, elems a, assocs a)
 in
 print (dump a1) >>
 print (dump a2)

