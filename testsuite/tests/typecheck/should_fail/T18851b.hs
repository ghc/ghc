{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances,
             ScopedTypeVariables, TypeFamilies, TypeApplications, NoPolyKinds,
             FlexibleContexts, AllowAmbiguousTypes #-}

module T18851b where

-- NB: -XNoPolyKinds is important. Otherwise, we get IsInt :: forall k. k -> Constraint,
-- but its instance specializes k to Type. The [W] IsInt int doesn't match the instance
-- then, and so we get no int ~ Int equality.

class C a b | a -> b
instance C Int b => C Int b

class IsInt int
instance int ~ Int => IsInt int

data A
instance Show A where
  show _ = "A"
data B
instance Show B where
  show _ = "B"

f :: forall a b c int. (Show a, Show b, Show c, C int a, C int b, C int c, IsInt int) => String
f = show (undefined :: c)

g = f @A @B
