{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZapDeps where

import Data.Kind ( Type )

withSomeSing :: forall k. (F (F k) ~ k) => F k -> ()
withSomeSing _ = undefined

type HList :: [Type] -> Type
data HList xs where
  HCons :: x -> HList '[x]

type F :: Type -> Type
type family F k
type instance F Type = Type
type instance F [a] = [FX a]
type instance F (HList xs) = HList (FX xs)

type FX :: forall k. k -> F k
type family FX a
type instance FX @Type a = F a
type instance FX @[k] (x:xs) = FX x : FX xs

myToSing :: FX (FX xs) ~ xs => F (HList xs) -> ()
myToSing (HCons x) = withSomeSing x
