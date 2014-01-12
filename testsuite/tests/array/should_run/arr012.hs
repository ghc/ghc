-- !!! Array map operations
-- 
--
module Main(main) where

import Data.Array
import Data.Char

main = 
 let 
  a1 = array (0,10) (zip [0..10] ['a'..'z'])
 in
 print a1 >>
 print (fmap (toUpper) a1) >>
 print (ixmap (3,8) (+1) a1)




