-- !!! Duplicate class assertion warning

-- ghc 5.05 removes duplicate class assertions,
-- mainly so that for-all-hoisting doesn't give
-- such duplicates... so this test now compiles
-- without warnings

module ShouldSucceed where

class Eq' a where
 doubleeq :: a -> a -> Bool

class (Eq' a) => Ord' a where
 lt :: a -> a -> Bool

instance Eq' Int where
 doubleeq x y = True

instance (Eq' a, Eq' a) => Eq' [a] where
 doubleeq x y = True

f x y = doubleeq x [1]
