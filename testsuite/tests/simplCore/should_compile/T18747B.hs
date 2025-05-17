{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T18747B where

import Data.Kind
import Data.Type.Equality

type family Sing :: k -> Type

data SomeSing (k :: Type) where
  SomeSing :: Sing (a :: k) -> SomeSing k

type family Promote (k :: Type) :: Type
type family PromoteX (a :: k) :: Promote k

type family Demote (k :: Type) :: Type
type family DemoteX (a :: k) :: Demote k

type SingKindX (a :: k) = (PromoteX (DemoteX a) ~~ a)

class SingKindX k => SingKind k where
  toSing :: Demote k -> SomeSing k

type instance Demote Type = Type
type instance Promote Type = Type
type instance DemoteX (a :: Type) = Demote a
type instance PromoteX (a :: Type) = Promote a

type instance Demote Bool = Bool
type instance Promote Bool = Bool

data Foo (a :: Type) where MkFoo :: Foo Bool

data SFoo :: forall a. Foo a -> Type where
  SMkFoo :: SFoo MkFoo
type instance Sing @(Foo _) = SFoo

type instance Demote (Foo a) = Foo (DemoteX a)
type instance Promote (Foo a) = Foo (PromoteX a)

instance SingKindX a => SingKind (Foo a) where
  toSing MkFoo = SomeSing SMkFoo

