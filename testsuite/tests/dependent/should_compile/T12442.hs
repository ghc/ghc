-- Based on https://github.com/idris-lang/Idris-dev/blob/v0.9.10/libs/effects/Effects.idr

{-# LANGUAGE DataKinds, PolyKinds, ScopedTypeVariables, TypeOperators,
             TypeApplications, GADTs, TypeFamilies, AllowAmbiguousTypes #-}

module T12442 where

import Data.Kind

data family Sing (a :: k)
data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>
type family (a :: k1 ~> k2) @@ (b :: k1) :: k2

data TyCon1 :: (Type -> Type) -> (Type ~> Type)
type instance (TyCon1 t) @@ x = t x

data TyCon2 :: (Type -> Type -> Type) -> (Type ~> Type ~> Type)
type instance (TyCon2 t) @@ x = (TyCon1 (t x))

data TyCon3 :: (Type -> Type -> Type -> Type) -> (Type ~> Type ~> Type ~> Type)
type instance (TyCon3 t) @@ x = (TyCon2 (t x))

type Effect = Type ~> Type ~> Type ~> Type

data EFFECT :: Type where
  MkEff :: Type -> Effect -> EFFECT

data EffElem :: (Type ~> Type ~> Type ~> Type) -> Type -> [EFFECT] -> Type where
  Here :: EffElem x a (MkEff a x ': xs)

data instance Sing (elem :: EffElem x a xs) where
  SHere :: Sing Here

type family UpdateResTy (b :: Type) (t :: Type)
                        (xs :: [EFFECT]) (elem :: EffElem e a xs)
                        (thing :: e @@ a @@ b @@ t) :: [EFFECT] where
  UpdateResTy b _ (MkEff a e ': xs) Here n = MkEff b e ': xs

data EffM :: (Type ~> Type) -> [EFFECT] -> [EFFECT] -> Type -> Type

effect :: forall e a b t xs prf eff m.
          Sing (prf :: EffElem e a xs)
       -> Sing (eff :: e @@ a @@ b @@ t)
       -> EffM m xs (UpdateResTy b t xs prf eff) t
effect = undefined

data State :: Type -> Type -> Type -> Type where
  Get :: State a a a

data instance Sing (e :: State a b c) where
  SGet :: Sing Get

type STATE t = MkEff t (TyCon3 State)

get_ :: forall m x. EffM m '[STATE x] '[STATE x] x
get_ = effect @(TyCon3 State) SHere SGet
