{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances,
             ScopedTypeVariables, TypeFamilies, TypeApplications,
             FlexibleContexts, AllowAmbiguousTypes, ExtendedDefaultRules #-}

module T18851f where

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
g = show (undefined :: D) ++ inner True
  where
    inner :: forall x. Eq x => x -> String
    inner _ = f @A @B
