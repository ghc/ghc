-- !!! Array creation, (index,value) list with duplicates.
-- 
-- Haskell library report 1.3 (and earlier) specifies
-- that `array' values created with lists containing dups,
-- are undefined ( _|_ ).
--
-- GHC-2.02 (and earlier) does not flag this as such, the
-- last (index,value) is instead used.
--
-- The report also specifies `array' is spine strict in
-- the (index,value) list argument and to check the
-- validity of the index values upon creation, it also
-- strict for the indices. To test this, we do (a!1)
-- twice, expecting to see the same value..
--
import Data.Array

main =
 let a1 = array (1,3) (zip (1:[1..3]) ['a'..'d']) in
 print (a1!1) >>
 print a1     >>
 print (a1!1)

