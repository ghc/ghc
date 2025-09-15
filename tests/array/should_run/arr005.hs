-- !!! Array - recurrences
-- 
-- array does not evaluate the elements.
--
import Data.Array

main =
 let 
  a1 = array (1,100) ((1,1::Integer):[(i,i*a1!(i-1))|i<-[2..100]])
 in
 print a1

--



