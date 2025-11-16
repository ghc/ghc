{-# LANGUAGE GADTs #-}

module T26582 where

sametype :: a -> a -> Int
sametype = sametype

f :: Eq a => (a->Int) -> Int
f = f

data T b where T1 :: T Bool

g1 :: T b -> Int
g1 v = f (\x -> case v of { T1 -> sametype x True })

g2 :: Eq c => c -> T b -> Int
g2 c v = f (\x -> case v of { T1 -> sametype x c })

{- The point is that we get something like

     Wanted: [W] d : Eq alpha[1]
     Implication
       level:  2
       Given:  b~Bool

       Wanted: [W] alpha[1]~Bool       -- For g1
       Wanted: [W] alpha[1]~c          -- For g2

So alpha is untouchable under the (b~Bool) from the GADT.
And yet in the end it's easy to solve
via alpha:=Bool, or alpha:=c resp

But having done that defaulting we must then remember to
solved that `d : Eq alpha`!  We forgot to so so in #26582.
-}
