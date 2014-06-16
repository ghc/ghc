{-# LANGUAGE RankNTypes #-}

-- Test silly type synonyms

module ShouldCompile where

type C u a = u	-- Note 'a' unused

foo :: (forall a. C u a -> C u a) -> u
foo x = undefined x

bar :: Num u => u
bar = foo (\t -> t + t)
-- The (Num u) should not get trapped inside the
-- /\a-abstraction which the compiler constructs for
-- the arg to foo.  But it might because it's Num (C u a)!

-- This test tickles a bizarre corner case documented
-- as [Silly Type Synonym] in TcMType.lhs
