-- !!! Array - empty arrays
-- 
-- print a couple of them to try to expose empty arrays 
-- to a GC or two.
import Data.Array

main =
 let 
  a1 = array (1,0) []
 in
 print (take 300 $ repeat (a1 :: Array Int Int))
