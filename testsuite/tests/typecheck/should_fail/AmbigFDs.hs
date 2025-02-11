{-# LANGUAGE FunctionalDependencies, NoPolyKinds #-}
-- NB: No AllowAmbiguousTypes. The type isn't ambiguous, because of the fundeps.
-- But it is confusing, because we don't know whether b1 and b2 are really the
-- same or not.

-- Sept 25: used to fail but now succeeds, because we don't complain
--          about insolubles arising solely from fundeps

module AmbigFDs where

class C a b | a -> b

foo :: (C a b1, C a b2) => a -> Int
foo = undefined
