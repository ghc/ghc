{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module T15549b where

import Data.Kind (Type)

newtype Identity a = Identity a
newtype Par1 a = Par1 a

data family Sing :: forall k. k -> Type
data instance Sing :: forall k. k -> Type

type family Rep1 (f :: Type -> Type) :: Type -> Type
type instance Rep1 Identity = Par1

type family From1 (z :: f a) :: Rep1 f a
type instance From1 ('Identity x) = 'Par1 x

und :: a
und = und

f :: forall (a :: Type) (x :: Identity a).  Sing x
f = g
    where g :: forall (a :: Type) (f :: Type -> Type) (x :: f a). Sing x
          g = seq (und :: Sing (From1 x)) und
