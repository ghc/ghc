{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds, MagicHash #-}

module T25647d_fail where

import GHC.Exts
import Data.Kind
import GHC.Exts (RuntimeRep)
import Data.Type.Equality ((:~:)(Refl) )

type Cast3 :: forall (r :: RuntimeRep) (s :: RuntimeRep) (a :: RuntimeRep) (b :: RuntimeRep) -> (a :~: IntRep) -> (b :~: IntRep) -> Type -> Type
type family Cast3 r s a b c d p where
  forall. Cast3 _ c _ b Refl Refl (p->q) = Int

type Cast4 :: forall (r :: RuntimeRep) (s :: RuntimeRep) (a :: RuntimeRep) (b :: RuntimeRep) -> (a :~: IntRep) -> (b :~: IntRep) -> Type -> Type
type family Cast4 r s a b c d p where
  forall aa cc b p q. Cast4 aa cc _ b Refl Refl (p->q) = Int
