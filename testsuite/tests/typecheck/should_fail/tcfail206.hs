{-# LANGUAGE TupleSections, UnboxedTuples #-}
module ShouldCompile where

a :: Bool -> (Int, Bool)
a = ( , True)

b :: Int -> Bool -> (Int, Bool)
b = (1, )

c :: a -> (a, Bool)
c = (True || False, )

d :: Bool -> (#Int, Bool#)
d = (# , True#)

e :: Int -> Bool -> (#Int, Bool#)
e = (#1, #)

f :: a -> (#a, Bool#)
f = (#True || False, #)
