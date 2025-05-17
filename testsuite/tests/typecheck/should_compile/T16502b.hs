{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module T16502b where

import Data.Kind

type family Sing :: k -> Type

class    (forall (sing :: k -> Type). sing ~ Sing => Show (sing z))
                       => ShowSing' (z :: k)
instance Show (Sing z) => ShowSing' (z :: k)

class    (forall (z :: k). ShowSing' z) => ShowSing k
instance (forall (z :: k). ShowSing' z) => ShowSing k

newtype Foo a = MkFoo a
data SFoo :: forall a. Foo a -> Type where
  SMkFoo :: Sing x -> SFoo (MkFoo x)
type instance Sing @(Foo _) = SFoo
deriving instance ShowSing a => Show (SFoo (z :: Foo a))

newtype Bar a = MkBar (Foo a)
data SBar :: forall a. Bar a -> Type where
  SMkBar :: forall a (x :: Foo a). Sing x -> SBar (MkBar x)
type instance Sing @(Bar _) = SBar
deriving instance ShowSing (Foo a) => Show (SBar (z :: Bar a))
