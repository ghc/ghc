{-# LANGUAGE Modifiers #-}
{-# LANGUAGE DataKinds, GADTs, PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
module SingletonsArrowProvider where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M)

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b %M -> Type
data TyCon1 :: (k1 -> k2) -> (k1 ~> k2)

type family Promote (k :: Type) :: Type
type family PromoteX (a :: k) :: Promote k
type instance Promote Type = Type
type instance PromoteX (a :: Type) = Promote a
type instance Promote (a -> b) = PromoteX a ~> PromoteX b
type instance Promote (a %M -> b) = PromoteX a ~> PromoteX b

-- | The bare equation still handles a term-level arrow.
runtime :: Promote (Type -> Type) :~: (Type ~> Type)
runtime = Refl
