{-# LANGUAGE GADTs #-}

-- From the ghc-users mailing list

module Foo where

data TValue t where
  TList :: [a] -> TValue [a] 

instance (Eq b) => Eq (TValue b) where
    (==) (TList p) (TList q) = (==) p q

{- My reply to the list

Here's the reasoning (I have done a bit of renaming).

* The TList constructor really has type
	TList :: forall a. forall x. (a~[x]) => [x] -> TValue a

* So in the pattern match we have
	(Eq b) available from the instance header
	TList p :: TValue b
	x is a skolem, existentially bound by the pattern
	p :: [x]
	b ~ [x] available from the pattern match

* On the RHS we find we need (Eq [x]).

* So the constraint problem we have is
	(Eq b, b~[x]) => Eq [x]
          ["Given"  =>  "Wanted"]
  Can we prove this?  From the two given constraints we can see
  that we also have Eq [x], and that certainly proves Eq [x].


Nevertheless, it's a bit delicate.  If we didn't notice all the
consequences of the "given" constraints, we might use the top-level Eq
a => Eq [a] instance to solve the wanted Eq [x].  And now we need Eq
x, which *isn't* a consequence of (Eq b, b~[x]).

-}