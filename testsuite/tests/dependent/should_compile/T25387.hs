{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TypeFamilies #-}
module Bug (f) where

import Data.Kind (Type)
import Data.Type.Equality (type (~~))

type Promote :: Type -> Type
type family Promote k

type PromoteX :: k -> Promote k
type family PromoteX a

type Demote :: Type -> Type
type family Demote (k :: Type) :: Type

type DemoteX :: k -> Demote k
type family DemoteX a

type HEq :: j -> k -> Type
data HEq a b where
  HRefl :: forall j (a :: j). HEq a a

type SHEq :: forall j k (a :: j) (b :: k). HEq a b -> Type
data SHEq heq where
  SHRefl :: forall j (a :: j). SHEq (HRefl @j @a)

type SomeSHEq :: j -> k -> Type
data SomeSHEq a b where
  SomeSHEq :: forall j k (a :: j) (b :: k) (heq :: HEq a b). SHEq heq -> SomeSHEq a b

f :: forall j k (a :: j) (b :: k).
     (PromoteX (DemoteX a) ~~ a, PromoteX (DemoteX b) ~~ b) =>
     HEq (DemoteX a) (DemoteX b) ->
     SomeSHEq a b
f HRefl = SomeSHEq SHRefl
