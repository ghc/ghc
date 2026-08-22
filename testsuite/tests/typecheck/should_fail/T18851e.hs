{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances,
             ScopedTypeVariables, TypeFamilies, TypeApplications,
             FlexibleContexts, AllowAmbiguousTypes, ExtendedDefaultRules #-}

module T18851e where

default (Int)

type family C_FD a
class C_FD a ~ b => C a b

type instance C_FD Int = Bool -- just for Show (C_FD Int)
instance C Int b => C Int b

class IsInt int
instance int ~ Int => IsInt int

data A
instance Show A where
  show _ = "A"
data B
instance Show B where
  show _ = "B"

data D

f :: forall a b c int
  .  ( Show c, Num int
     , C int a, C int b, C int c
     -- , c ~ C_FD int -- add this to get rid of ambiguity error
     )
  => String
f = show (undefined :: c)

g :: String
g = f @A @B  ++ show (undefined :: D)
  -- This variant, suggested by Claude in a review of !16564,
  -- has an unsolved (Show D) constraint.  It must not be suppressed
  -- by the insoluble-but-suppressed constraints arising from (C Int a)
