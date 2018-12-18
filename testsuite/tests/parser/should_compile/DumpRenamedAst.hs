{-# LANGUAGE DataKinds, GADTs, PolyKinds, RankNTypes, TypeOperators,
             TypeFamilies, StarIsType, TypeApplications #-}

module DumpRenamedAst where
import Data.Kind

import Data.Kind (Type)

data Peano = Zero | Succ Peano

type family Length (as :: [k]) :: Peano where
  Length (a : as) = Succ (Length as)
  Length '[]      = Zero

data family Nat :: k -> k -> Type
-- Ensure that the `k` in the type pattern and `k` in the kind signature have
-- the same binding site.
newtype instance Nat (a :: k -> Type) :: (k -> Type) -> Type where
  Nat :: (forall xx. f xx -> g xx) -> Nat f g

data T f (a :: k) = MkT (f a)

type family F1 (a :: k) (f :: k -> Type) :: Type where
  F1 @Peano a f = T @Peano f a

main = putStrLn "hello"
