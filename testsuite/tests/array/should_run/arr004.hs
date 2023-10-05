-- !!! Array - accessing undefined element
-- 
-- Sample Haskell implementation in the 1.3 Lib report defines
-- this as being undefined/error.

import Data.Array

main =
 let a1 = array (1,3) (zip ([1,2]) ['a'..'d']) in
 print (a1!3)

-- output: Fail: (Array.!): undefined array element



