-- !!! Duplicate class assertion warning

-- ghc 6.6 now warns about duplicate class assertions,

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
