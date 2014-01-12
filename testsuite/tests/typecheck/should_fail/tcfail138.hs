{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, UndecidableInstances #-}
-- UndecidableInstances because (L a b) is no smaller than (C a b)

-- This one shows up another rather subtle functional-dependecy
-- case.  The error is:
--
--    Could not deduce (C a b') from the context (C a b)
--      arising from the superclasses of an instance declaration at Foo.hs:8:0
--    Probable fix: add (C a b') to the instance declaration superclass context
--    In the instance declaration for `C (Maybe a) a'
--
-- Since L is a superclass of the (sought) constraint (C a b'), you might
-- think that we'd generate the superclasses (L a b') and (L a b), and now 
-- the fundep will force b=b'.  But GHC is very cautious about generating
-- superclasses when doing context reduction for instance declarations,
-- becasue of the danger of superclass loops.
--
-- So, today, this program fails.  It's trivial to fix by adding a fundep for C
-- 	class (G a, L a b) => C a b | a -> b

-- Note: Sept 08: when fixing Trac #1470, tc138 started working! 
-- This test is a very strange one (fundeps, undecidable instances), 
-- so I'm just marking it as "should-succeed".  It's not very clear to
-- me what the "right" answer should be; when we have the type equality
-- story more worked out we might want to think about that.

module ShouldFail where

class G a
class L a b | a -> b
class (G a, L a b) => C a b

instance C a b' => G (Maybe a)
instance C a b  => C (Maybe a) a
instance L (Maybe a) a
