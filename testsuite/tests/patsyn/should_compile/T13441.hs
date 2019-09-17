{-# LANGUAGE ScopedTypeVariables, PatternSynonyms, DataKinds, PolyKinds,
             GADTs, TypeOperators, TypeFamilies, TypeFamilyDependencies #-}

module T13441 where

import Data.Functor.Identity

data FList f xs where
  FNil :: FList f '[]
  (:@) :: f x -> FList f xs -> FList f (x ': xs)

data Nat = Zero | Succ Nat

type family Length (xs :: [k]) :: Nat where
  Length '[]       = Zero
  Length (_ ': xs) = Succ (Length xs)

type family Replicate (n :: Nat) (x :: a) = (r :: [a]) where
  Replicate Zero     _ = '[]
  Replicate (Succ n) x = x ': Replicate n x

type Vec n a = FList Identity (Replicate n a)

-- Using explicitly-bidirectional pattern
pattern (:>) :: forall n a. n ~ Length (Replicate n a)
             => forall m. n ~ Succ m
             => a -> Vec m a -> Vec n a
pattern x :> xs <- Identity x :@ xs
  where
    x :> xs = Identity x :@ xs

-- Using implicitly-bidirectional pattern
pattern (:>>) :: forall n a. n ~ Length (Replicate n a)
             => forall m. n ~ Succ m
             => a -> Vec m a -> Vec n a
pattern x :>> xs = Identity x :@ xs
