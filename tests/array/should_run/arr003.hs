-- !!! Array creation, (index,value) list with out of bound index.
-- 
-- Haskell library report 1.3 (and earlier) specifies
-- that `array' values created with lists containing out-of-bounds indices,
-- are undefined ( _|_ ).
--
-- GHC implementation of `array' catches this (or, rather, 
-- `index' does) - the argument list to `array' is defined
-- to have its spine be evaluated - so the indexing below
-- should cause a failure.
--
import Data.Array

main =
 let a1 = array (1::Int,3) (zip ([1..4]) ['a'..'d']) in
 print (a1!2)
