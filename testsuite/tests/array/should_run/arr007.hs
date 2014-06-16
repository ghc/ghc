-- !!! Array - accessing empty arrays
-- 
-- empty arrays are legal, but indexing them is undefined!
--
import Data.Array

main =
 let 
  a1 = array (1::Int,0) [(1,'a')]
 in
 print (a1!0)
