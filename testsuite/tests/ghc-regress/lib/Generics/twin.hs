{-# OPTIONS -fglasgow-exts #-}
 
{-

For the discussion in the 2nd boilerplate paper,
we favour some simplified development of twin traversal.
So the full general, stepwise story is in Data.Generics.Twin,
but the short version from the paper is turned into a test
case below. 

See the paper for an explanation.
 
-}

module Main where
import Data.Generics hiding (GQ,gzipWithQ,geq)

geq' :: GenericQ (GenericQ Bool)
geq' x y =  toConstr x == toConstr y
         && and (gzipWithQ geq' x y)

geq :: Data a => a -> a -> Bool
geq = geq'

newtype GQ r = GQ (GenericQ r)

gzipWithQ :: GenericQ (GenericQ r)
          -> GenericQ (GenericQ [r])
gzipWithQ f t1 t2 
    = gApplyQ (gmapQ (\x -> GQ (f x)) t1) t2

gApplyQ :: Data a => [GQ r] -> a -> [r]
gApplyQ qs t = reverse (snd (gfoldlQ k z t))
    where
      k (GQ q : qs, rs) child = (qs, q child : rs)
      z = (qs, [])

newtype R r x = R { unR :: r }

gfoldlQ :: (r -> GenericQ r)
        -> r 
        -> GenericQ r

gfoldlQ k z t = unR (gfoldl k' z' t)
    where
      z' _ = R z
      k' (R r) c = R (k r c)

main = print $ ( geq [True,True] [True,True]
               , geq [True,True] [True,False]
               )
