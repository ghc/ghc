
-- Killed 6.2.2
-- The trouble was that 1 was instantiated to a type (t::?)
-- and the constraint (Foo (t::? -> s::*)) didn't match Foo (a::* -> b::*).
-- Solution is to zap the expected type in TcEpxr.tc_expr(HsOverLit). 

module ShoudlCompile where

class Foo a where
     foo :: a

instance Foo (a -> b) where
     foo = error "urk"

test :: ()
test = foo 1
