{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds, MagicHash #-}

module T25647d where

import GHC.Exts
import Data.Kind
import GHC.Exts (RuntimeRep)
import Data.Type.Equality ((:~:)(Refl) )

type Cast0 :: forall (r :: RuntimeRep) (s :: RuntimeRep) (a :: RuntimeRep) (b :: RuntimeRep) -> (a :~: IntRep) -> (b :~: IntRep) -> Type -> Type
type family Cast0 r s a b c d p where
  Cast0 _ c _ _ Refl Refl (p->q) = Int

type Cast1 :: forall (r :: RuntimeRep) (s :: RuntimeRep) (a :: RuntimeRep) (b :: RuntimeRep) -> (a :~: IntRep) -> (b :~: IntRep) -> Type -> Type
type family Cast1 r s a b c d p where
  Cast1 _ c _ b Refl Refl (p->q) = Int

type Cast2 :: forall (r :: RuntimeRep) (s :: RuntimeRep) (a :: RuntimeRep) (b :: RuntimeRep) -> (a :~: a) -> (b :~: b) -> Type -> Type
type family Cast2 r s a b c d p where
  forall c b p q.Cast2 _ c _ b Refl Refl (p->q) = Int
