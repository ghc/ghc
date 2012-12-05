{-# LANGUAGE RankNTypes #-}

-- The interesting thign about this one is that
-- there's an unbound type variable of kind *->*
-- that the typechecker should default to some 
-- arbitrary type.
--
-- GHC 5.02 could only really deal with such things
-- of kind *, but 5.03 extended that to *->..->*
-- Still not complete, but a lot better.

module ShouldCompile where

f :: (forall a b . a b -> int) -> (forall c . c int) -> int 
f x y = x y
