-- A simple GADT test from Roman
-- which nevertheless showed up a bug at one stage

module ShouldCompile where

data T a where
  T1 :: () -> T ()
  T2 :: T a -> T b -> T (a,b)

class C a where
  f :: T a -> a

instance C () where
  f (T1 x) = x

instance (C a, C b) => C (a,b) where
  f (T2 x y) = (f x, f y)
