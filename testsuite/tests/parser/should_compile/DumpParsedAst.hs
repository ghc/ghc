{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds, GADTs, PolyKinds, RankNTypes, TypeOperators, TypeFamilies
             , TypeApplications #-}

module DumpParsedAst where
import Data.Kind

data Peano = Zero | Succ Peano

type family Length (as :: [k]) :: Peano where
  Length (a : as) = Succ (Length as)
  Length '[]      = Zero

-- vis kind app
data T f (a :: k) = MkT (f a)

type family F1 (a :: k) (f :: k -> Type) :: Type where
  F1 @Peano a f = T @Peano f a

data family Nat :: k -> k -> Type
newtype instance Nat (a :: k -> Type) :: (k -> Type) -> Type where
  Nat :: (forall xx. f xx -> g xx) -> Nat f g

main = putStrLn "hello"
