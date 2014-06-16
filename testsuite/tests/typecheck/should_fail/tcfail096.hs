{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}
module ShouldFail where

class Foo f a r | f a -> r where
      foo::f->a->r

-- These instances are incompatible because we can unify
-- the first two paramters, though it's rather obscure:
--	p -> (a,b)
--	t -> (,) (a,a)
--	c -> (,) a
--	r -> s
--
-- So a constraint which would sow this up is
--	Foo ((Int,Int)->Int)
--	    ((Int,Int), (Int,Int))
--	    t
-- This matches both.  Not easy to spot, and the error
-- message would be improved by giving the unifier, or
-- a witness.

instance Foo (p->s)     (t p)    (t s)
instance Foo ((a,b)->r) (c a,c b)(c r)

