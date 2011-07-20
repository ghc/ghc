import Data.Array -- 1.3

val1, val2 :: Array (Int,Int) Int
val1 = array ((1,2), (2,1)) []
val2 = array ((2,1), (1,2)) []
 
val3 :: Array Integer Double
val3 = array (4, -3) []

main = print ((val1 == val1) && (val2 == val2) && (val3 == val3))
