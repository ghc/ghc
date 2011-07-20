-- !!! Simple array creation

import Data.Array

main =
 let a1 = array (1,3) (zip [2,3,1] ['a'..'d']) in
 print a1

-- Result:
