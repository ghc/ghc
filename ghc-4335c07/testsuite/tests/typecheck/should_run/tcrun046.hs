{-# LANGUAGE TypeFamilies, ConstraintKinds, GADTs #-}

module Main where

-- Here we abstract over a *constraint*!
-- T :: Constraint -> *

data T a where
  MkT :: a => T a

f :: T (Ord x) -> x -> Bool
f MkT x = x > x

g :: T (Ord Int)
g = MkT

main = print (f g 4)
