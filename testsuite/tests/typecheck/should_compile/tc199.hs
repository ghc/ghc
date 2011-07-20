{-# LANGUAGE MultiParamTypeClasses #-}

-- This code defines a default method with a highly dubious type,
-- because 'v' is not mentioned, and there are no fundeps
--
-- However, arguably the instance declaration should be accepted,
-- beause it's equivalent to 
--	instance Baz Int Int where { foo x = x }
-- which *does* typecheck

-- GHC does not actually macro-expand the instance decl.  Instead, it
-- defines a default method function, thus
--
--	$dmfoo :: Baz v x => x -> x
--	$dmfoo y = y
--
-- Notice that this is an ambiguous type: you can't call $dmfoo
-- without triggering an error.  And when you write an instance decl,
-- it calls the default method:
--
--	instance Baz Int Int where foo = $dmfoo
--
-- I'd never thought of that.  You might think that we should just
-- *infer* the type of the default method (here forall a. a->a), but
-- in the presence of higher rank types etc we can't necessarily do
-- that.

module Foo1 where

class Baz v x where
   foo :: x -> x
   foo y = y

instance Baz Int Int 
