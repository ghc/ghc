{-# LANGUAGE PatternSynonyms #-}
module PolyPat where

-- Testing whether type changing updates work correctly.

pattern MyTuple :: a -> b -> (a, b)
pattern MyTuple{mfst, msnd} = (mfst, msnd)


expr1 :: (Int, String) -> (Int, Int)
expr1 a = a { msnd = 2}

expr3 a = a { msnd = 2}

expr2 :: (a, b) -> a
expr2 a = mfst a
