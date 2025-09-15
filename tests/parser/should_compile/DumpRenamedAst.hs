{-# LANGUAGE DataKinds, GADTs, PolyKinds, RankNTypes, TypeOperators,
             TypeFamilies, StarIsType, TypeApplications #-}

module DumpRenamedAst where
import Data.Kind

import Data.Kind (Type)

data Peano = Zero | Succ Peano

type Length :: [k] -> Peano
type family Length (as :: [k]) :: Peano where
  Length (a : as) = Succ (Length as)
  Length '[]      = Zero

data family Nat :: k -> k -> Type
-- Ensure that the `k` in the type pattern and `k` in the kind signature have
-- the same binding site.
newtype instance Nat (a :: k -> Type) :: (k -> Type) -> Type where
  Nat :: (forall xx. f xx -> g xx) -> Nat f g

data T f (a :: k) = MkT (f a)

type F1 :: k -> (k -> Type) -> Type
type family F1 (a :: k) (f :: k -> Type) :: Type where
  F1 @Peano a f = T @Peano f a

class C a where
  type F a b

instance C [a] where
  type F [a] b = Either a b -- Ensure that the HsOuterImplicit for the F
                            -- instance only quantifies over `b` (#19649)

main = putStrLn "hello"
