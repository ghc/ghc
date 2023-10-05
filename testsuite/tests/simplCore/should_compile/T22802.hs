{-# OPTIONS_GHC -O1 #-}
module T22802 where

class C a where
  f :: a -> a -> a
  g :: a -> a -> a
instance C () where
  f = g
  g = f

h :: a -> () -> ()
h = mapFB f (const ())

mapFB :: (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
{-# INLINE [0] mapFB #-}
mapFB c f = \x ys -> c (f x) ys

{-# RULES
"my-mapFB" forall c a b. mapFB (mapFB c a) b = mapFB c (a.b)
  #-}
