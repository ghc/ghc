{-# LANGUAGE FunctionalDependencies, NoPolyKinds #-}
-- NB: No AllowAmbiguousTypes. The type isn't ambiguous, because of the fundeps.
-- But it is confusing, because we don't know whether b1 and b2 are really the
-- same or not.

module AmbigFDs where

class C a b | a -> b

foo :: (C a b1, C a b2) => a -> Int
foo = undefined
