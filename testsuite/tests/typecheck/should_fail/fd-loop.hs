{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- Here's a nice example of a fundep loop, correctly
-- rejected by the undecidable-instance check.
-- See comments below.

module FDLoop where

class C a b | a -> b where f :: a -> b
newtype T a = T a

instance (C a b, Eq b) => Eq (T a) where (==) = undefined

g x = (undefined :: d -> d -> d -> ()) (T x) (f x) (undefined :: Eq e => e)

{-    Analysis

   f :: C a b => a -> b
   x :: a
   b ~ T a
   need: C a b
   b ~ e
   need: Eq e

Hence need (C a (T a), Eq (T a))
Apply instance for Eq
     = (C a (T a), C a g, Eq g)
Apply functional dependency: g ~ T a
     = (C a (T a), C a (T a), Eq (T a))
And now we are back where we started
-}

