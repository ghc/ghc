{-# LANGUAGE GADTs, RankNTypes, PolyKinds #-}
module T6093 where

-- Polymorphic kind recursion
data R :: forall k. k -> * where
    MkR :: R f -> R (f ())

data IOWitness (a :: k) = IOW

data Type :: forall k. k -> * where
  SimpleType :: IOWitness a -> Type a
  ConstructedType :: Type f -> Type a -> Type (f a)
