{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module T13790 where

import Data.Kind

data family Sing (a :: k)

data SomeSing (k :: Type) where
  SomeSing :: Sing (a :: k) -> SomeSing k

type family Promote k :: Type

class (Promote (Demote k) ~ k) => SingKind (k :: Type) where
  type Demote k :: Type
  fromSing :: Sing (a :: k) -> Demote k
  toSing :: Demote k -> SomeSing k

type family DemoteX (a :: k) :: Demote k
type instance DemoteX (a :: Type) = Demote a

type instance Promote Type = Type

instance SingKind Type where
  type Demote Type = Type
  fromSing = error "fromSing Type"
  toSing   = error "toSing Type"

-----

data N = Z | S N

data instance Sing (z :: N) where
  SZ :: Sing Z
  SS :: Sing n -> Sing (S n)
type instance Promote N = N

instance SingKind N where
  type Demote N = N
  fromSing SZ = Z
  fromSing (SS n) = S (fromSing n)
  toSing Z = SomeSing SZ
  toSing (S n) = case toSing n of
                   SomeSing sn -> SomeSing (SS sn)

type instance DemoteX (n :: N) = n
