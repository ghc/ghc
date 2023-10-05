{-# LANGUAGE GADTs, RankNTypes, PolyKinds #-}
module T6093 where

import qualified Data.Kind as K (Type)

-- Polymorphic kind recursion
data R :: forall k. k -> K.Type where
    MkR :: R f -> R (f ())

data IOWitness (a :: k) = IOW

data Type :: forall k. k -> K.Type where
  SimpleType :: IOWitness a -> Type a
  ConstructedType :: Type f -> Type a -> Type (f a)
