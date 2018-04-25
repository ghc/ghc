-- The functions in this file are expressly for the purpose of aiding
-- the typesetting of some functions with Smugweb.  To this end, in
-- some cases I will use named, prefix functions rather than operators
-- (since under Smugweb operators cannot accept arguments).  This file
-- will define those infix functions.

module TypesettingTricks where

realdiv:: Floating a => a -> a -> a
realdiv = (/)

realmul:: Num a => a -> a -> a
realmul = (*)

dotmul:: Num a => a -> a -> a
dotmul = (*)

rand:: Integer -> [ Float ]
rand i = r : rand i'
   where i' = ( (3146757 * i) + 1731) `mod` 4194304
         r = (fromInteger i') / 4194304.0
