{-# LANGUAGE GADTs, PolyKinds #-}
module T6093 where

-- Polymorphic kind recursion
data R :: k -> * where
    MkR :: R f -> R (f ())


data IOWitness (a :: k) = IOW

data Type :: k -> * where
  SimpleType :: IOWitness a -> Type a
  ConstructedType :: Type f -> Type a -> Type (f a)
