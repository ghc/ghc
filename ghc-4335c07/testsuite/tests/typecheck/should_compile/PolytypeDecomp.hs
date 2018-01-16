{-# LANGUAGE TypeFamilies, LiberalTypeSynonyms, ImpredicativeTypes #-}
module PolyTypeDecomp where 


{- The purpose of this test is to check if decomposition of wanted 
   equalities in the /constraint solver/ (vs. the unifier) works properly.
   Unfortunately most equalities between polymorphic types are converted to 
   implication constraints early on in the unifier, so we have to make things
   a bit more convoluted by introducing the myLength function. The wanted 
   constraints we get for this program are:
      [forall a. Maybe a] ~ Id alpha
      [forall a. F [a]]   ~ Id alpha 
   Which, /after reactions/ should create a fresh implication: 
      forall a. Maybe a ~ F [a]
   that is perfectly soluble.
-}
 
type family F a
type instance F [a] = Maybe a 

type family Id a 
type instance Id a = a

f :: [forall a. F [a]]
f = undefined


g :: [forall a. Maybe a] -> Int
g x = myLength [x,f]

myLength :: [Id a] -> Int 
myLength = undefined
