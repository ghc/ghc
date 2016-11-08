module Rules where

import Data.Char

{-# RULES "map-loop" [ ~  ]  forall f . map' f = map' (id . f) #-}

{-# NOINLINE map' #-}
map' f [] = []
map' f (x:xs) = f x : map' f xs

main = print (map' toUpper "Hello, World")

-- Should warn
foo1 x = x
{-# RULES "foo1" [ 1] forall x. foo1 x = x #-}

-- Should warn
foo2 x = x
{-# INLINE foo2 #-}
{-# RULES "foo2" [~ 1 ] forall x. foo2 x = x #-}

-- Should not warn
foo3 x = x
{-# NOINLINE foo3 #-}
{-# RULES "foo3" forall x. foo3 x = x #-}

{-# NOINLINE f #-}
f :: Int -> String
f x = "NOT FIRED"

{-# NOINLINE neg #-}
neg :: Int -> Int
neg = negate

{-# RULES
     "f" forall (c::Char->Int) (x::Char). f (c x) = "RULE FIRED"
 #-}
