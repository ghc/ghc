{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
module Bug where

import Data.Kind

-- type SingFunction2 f = forall t1 t2. Sing t1 -> Sing t2 -> Sing (f `Apply` t1 `Apply` t2)
type SingFunction2 f = forall t1. Sing t1 -> forall t2. Sing t2 -> Sing (f `Apply` t1 `Apply` t2)
singFun2 :: forall f. SingFunction2 f -> Sing f
singFun2 f = SLambda (\x -> SLambda (f x))

type family Sing :: k -> Type
data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

type family Apply (f :: a ~> b) (x :: a) :: b
data Sym4 a
data Sym3 a

type instance Apply Sym3 _ = Sym4

newtype SLambda (f :: k1 ~> k2) =
  SLambda { applySing :: forall t. Sing t -> Sing (Apply f t) }
type instance Sing = SLambda

und :: a
und = undefined

data E
data ShowCharSym0 :: E ~> E ~> E

sShow_tuple :: SLambda Sym4
sShow_tuple
  = applySing (singFun2 @Sym3 und)
          (und (singFun2 @Sym3
                 (und (applySing (singFun2 @Sym3 und)
                                 (applySing (singFun2 @ShowCharSym0 und) und)))))
