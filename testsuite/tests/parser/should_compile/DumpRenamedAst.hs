{-# LANGUAGE DataKinds, GADTs, PolyKinds, RankNTypes, TypeOperators,
             TypeFamilies #-}

module DumpRenamedAst where

data Peano = Zero | Succ Peano

type family Length (as :: [k]) :: Peano where
  Length (a : as) = Succ (Length as)
  Length '[]      = Zero

data family Nat :: k -> k -> *
-- Ensure that the `k` in the type pattern and `k` in the kind signature have
-- the same binding site.
newtype instance Nat (a :: k -> *) :: (k -> *) -> * where
  Nat :: (forall xx. f xx -> g xx) -> Nat f g

main = putStrLn "hello"
