{-# LANGUAGE UndecidableInstances, FlexibleInstances,
             MultiParamTypeClasses, FunctionalDependencies #-}

-- Test for trac #816
-- GHC's typechecker loops when trying to type this, resulting in a
-- context stack overflow.

{- Maybe this should typecheck:

   Given: Foo x y, Bar y z
   Wanted: Foo x beta, Bar beta z

If we happened to process (Foo x beta) first we 
might generate the extra equality beta~y, and we are good

If we process (Bar beta z) first, we end up in an infinite
loop, using the (Bar x z) instance repeatedly.

If instead we'd had
   class (F x ~ y) => Foo x y where
     type F x
     foo :: x -> y

Then after canonicalising we get
     Given: Foo x y, Bar y z, F x ~ y
     Wanted: Foo x beta, Bar beta z
-}

module ShouldCompile where

class Foo x y | x -> y where
 foo :: x -> y

class Bar x z where
 bar :: x -> z -> Int

instance (Foo x y, Bar y z) => Bar x z where
 bar x z = bar (foo x) z

