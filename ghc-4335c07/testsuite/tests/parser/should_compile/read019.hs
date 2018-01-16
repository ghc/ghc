-- !!! Checking what's legal in the body of a class declaration.
module ShouldCompile where

class Foo a where {
  (--<>--) :: a -> a -> Int  ;
  infixl 5 --<>-- ;
  (--<>--) _ _ = 2 ; -- empty decl at the end.
};


