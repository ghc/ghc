{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
module Bug where

import Data.Kind
import Data.Type.Bool
import Data.Type.Equality ((:~:)(..))
import Data.Void

data family Sing :: forall k. k -> Type
data instance Sing :: Bool -> Type where
  SFalse :: Sing False
  STrue  :: Sing True
data instance Sing :: forall a. [a] -> Type where
  SNil  :: Sing '[]
  SCons :: Sing x -> Sing xs -> Sing (x:xs)
data instance Sing :: forall a b. (a, b) -> Type where
  STuple2 :: Sing x -> Sing y -> Sing '(x, y)
newtype instance Sing (f :: k1 ~> k2) =
  SLambda { (@@) :: forall t. Sing t -> Sing (f @@ t) }

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>
type family (f :: k1 ~> k2) @@ (x :: k1) :: k2
infixl 9 @@

newtype Map k v = MkMap [(k, v)]
data instance Sing :: forall k v. Map k v -> Type where
  SMkMap :: Sing x -> Sing (MkMap x)

type family MapEmpty :: Map k v where
  MapEmpty = MkMap '[]

type family MapInsertWith (f :: v ~> v ~> v) (new_k :: k) (new_v :: v) (m :: Map k v) :: Map k v where
  MapInsertWith _ new_k new_v (MkMap '[]) = MkMap '[ '(new_k, new_v)]
  MapInsertWith f new_k new_v (MkMap ('(old_k,old_v):old_kvs)) =
    If (old_k == new_k)
       (MkMap ('(new_k,f @@ new_v @@ old_v):old_kvs))
       (Case (MapInsertWith f new_k new_v (MkMap old_kvs)) old_k old_v)

type family Case (m :: Map k v) (old_k :: k) (old_v :: v) :: Map k v where
  Case (MkMap kvs) old_k old_v = MkMap ('(old_k,old_v) : kvs)

sMapInsertWith :: forall k v (f :: v ~> v ~> v) (new_k :: k) (new_v :: v) (m :: Map k v).
                  SEq k
               => Sing f -> Sing new_k -> Sing new_v -> Sing m
               -> Sing (MapInsertWith f new_k new_v m)
sMapInsertWith _  snew_k snew_v (SMkMap SNil) = SMkMap (STuple2 snew_k snew_v `SCons` SNil)
sMapInsertWith sf snew_k snew_v (SMkMap (STuple2 sold_k sold_v `SCons` sold_kvs)) =
  case sold_k %== snew_k of
    STrue -> SMkMap (STuple2 snew_k (sf @@ snew_v @@ sold_v) `SCons` sold_kvs)
    SFalse ->
      case sMapInsertWith sf snew_k snew_v (SMkMap sold_kvs) of
        SMkMap skvs -> SMkMap (STuple2 sold_k sold_v `SCons` skvs)

class PEq a where
  type (x :: a) == (y :: a) :: Bool
class SEq a where
  (%==) :: forall (x :: a) (y :: a).
           Sing x -> Sing y -> Sing (x == y)

mapInsertWithNonEmpty1 :: forall k v (f :: v ~> v ~> v) (old_k :: k) (old_v :: v) (old_kvs :: [(k,v)])
                                     (new_k :: k) (new_v :: v) (m :: Map k v).
                          SEq k
                       => Sing f -> Sing new_k -> Sing new_v -> Sing m
                       -> m :~: MkMap ('(old_k,old_v) : old_kvs)
                       -> MapInsertWith f new_k new_v m :~: MapEmpty
                       -> Void
mapInsertWithNonEmpty1 sf snew_k snew_v (SMkMap sm) Refl Refl
  | STuple2 sold_k _ `SCons` sold_kvs <- sm
  , SFalse <- sold_k %== snew_k
  = case sMapInsertWith sf snew_k snew_v (SMkMap sold_kvs) of {}

mapInsertWithNonEmpty2 :: forall k v (f :: v ~> v ~> v) (old_k :: k) (old_v :: v) (old_kvs :: [(k,v)])
                                     (new_k :: k) (new_v :: v) (m :: Map k v).
                          SEq k
                       => Sing f -> Sing new_k -> Sing new_v -> Sing m
                       -> m :~: MkMap ('(old_k,old_v) : old_kvs)
                       -> MapInsertWith f new_k new_v m :~: MapEmpty
                       -> Void
mapInsertWithNonEmpty2 sf snew_k snew_v (SMkMap sm) Refl Refl
  | STuple2 sold_k _ `SCons` sold_kvs <- sm
  = case sold_k %== snew_k of
      SFalse ->
        case sMapInsertWith sf snew_k snew_v (SMkMap sold_kvs) of {}

