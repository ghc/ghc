-- Gave a nasty
--	tcLookupGlobal: `FunnyError.$dmb' is not in scope
-- failure in GHC 6.2, because the type-checking of
-- the default method didn't recover.

module ShouldFail where

class A x where
   a :: x -> ()
   b :: x -> Bool -> ()
   b x "Foo" = () -- deliberate type error

instance A Int where
   a _ = ()
