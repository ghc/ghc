-- The type sig mentions a type variable that doesn't appear in 
-- the type.  This one killed GHC 5.03, in a trivial way.

module ShouldCompile where

type T a = () -> ()

f :: T a
f () = ()
