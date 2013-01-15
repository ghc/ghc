{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances,
             OverlappingInstances, UndecidableInstances #-}

-- Rather exotic example posted to Haskell mailing list 17 Oct 07
-- It concerns context reduction and functional dependencies

module FooModule where

class Concrete a b | a -> b where
	bar :: a -> String

class Wuggle b | -> b  -- To make the Concrete instance work

instance (Show a, Wuggle b) => Concrete a b where
	bar = error "urk"

wib :: Concrete a b => a -> String
wib x = bar x

-- Uncommenting this solves the problem:
-- instance Concrete Bool Bool

{- This is a nice example of the trickiness of functional dependencies.
Here's what is happening.

Consider type inference for 'wib'.  GHC 6.6 figures out that the call
of 'bar' gives rise to the constraint (Concrete p q), where x has type
'p'.  Ah, but x must have type 'a', so the constraint is (Concrete a
q).

Now GHC tries to satisfy (Concrete a q) from (Concrete a b). If it
applied improvement right away it'd succeed, but sadly it first looks
at instances declarations.  Success: we can get (Concrete a q) from
(Show a).  So it uses the instance decl and now we can't get (Show a)
from (Concrete a b).


OK,  found that in GHC 6.6, adding
	instance Concrete Bool Bool
fixed the problem. That's weird isn't it?  The reason is this. When GHC looks
at the instance decls, it now sees *two* instance decls matching
(Concrete a q), and so it declines for now to use either of them
(since it's not clear which would be the right one).  Once it has
finished with instance decls it tries improvement.  And, yes, it now
sees that q=b, so all is well.

You might say that GHC should use improvement more vigorously, and
perhaps you'd be right.  And indeed the upcoming GHC 6.8 does exactly
that.
-}

