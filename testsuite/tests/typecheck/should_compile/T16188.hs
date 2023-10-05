{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T16188 where

import Data.Kind (Type)
import Data.Type.Bool (type (&&))

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>
type family Apply (f :: a ~> b) (x :: a) :: b
data family Sing :: forall k. k -> Type

data instance Sing :: Bool -> Type where
  SFalse :: Sing False
  STrue  :: Sing True

(%&&) :: forall (x :: Bool) (y :: Bool).
         Sing x -> Sing y -> Sing (x && y)
SFalse %&& _ = SFalse
STrue  %&& a = a

data RegExp :: Type -> Type where
  App :: RegExp t -> RegExp t -> RegExp t

data instance Sing :: forall t. RegExp t -> Type where
  SApp :: Sing re1 -> Sing re2 -> Sing (App re1 re2)

data ReNotEmptySym0 :: forall t. RegExp t ~> Bool
type instance Apply ReNotEmptySym0 r = ReNotEmpty r

type family ReNotEmpty (r :: RegExp t) :: Bool where
  ReNotEmpty (App re1 re2) = ReNotEmpty re1 && ReNotEmpty re2

sReNotEmpty :: forall t (r :: RegExp t).
               Sing r -> Sing (Apply ReNotEmptySym0 r :: Bool)
sReNotEmpty (SApp sre1 sre2) = sReNotEmpty sre1 %&& sReNotEmpty sre2

blah :: forall (t :: Type) (re :: RegExp t).
        Sing re -> ()
blah (SApp sre1 sre2)
  = case (sReNotEmpty sre1, sReNotEmpty sre2) of
      (STrue, STrue) -> ()
