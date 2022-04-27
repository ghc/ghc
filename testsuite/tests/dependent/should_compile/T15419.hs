{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module T15419 where

import Data.Kind

data Prod a b
data Proxy p = Proxy

-----

data family Sing :: forall k. k -> Type
data instance Sing x = STuple

-----

type family Rep1 (f :: k -> Type) :: k -> Type
type instance Rep1 ((,) a) = Prod a

type family From1 (f :: Type -> Type) a (z :: f a) :: Rep1 f a
type family To1 (f :: Type -> Type) a (z :: Rep1 f a) :: f a

class Generic1 (f :: Type -> Type) where
  sFrom1 :: forall (a :: Type) (z :: f a).      Proxy z -> Sing (From1 f a z)
  sTo1   :: forall (a :: Type) (r :: Rep1 f a). Proxy r -> Proxy (To1 f a r :: f a)

instance Generic1 ((,) a) where
  sFrom1 Proxy = undefined
  sTo1   Proxy = undefined

-----

type family Fmap (g :: b) (x :: f a) :: f b
type instance Fmap (g :: b) (x :: (u, a)) = To1 ((,) u) b (Fmap g (From1 ((,) u) a x))

class PFunctor (f :: Type -> Type) where
  sFmap         :: forall a b (g :: b) (x :: f a).
                   Proxy g -> Sing x -> Proxy (Fmap g x)

instance PFunctor (Prod a) where
  sFmap _ STuple = undefined

sFmap1 :: forall (f :: Type -> Type) (u :: Type) (b :: Type) (g :: b) (x :: f u).
                 (Generic1 f,
                  PFunctor (Rep1 f),
                  Fmap g x ~ To1 f b (Fmap g (From1 f u x)) )
              => Proxy g -> Proxy x -> Proxy (Fmap g x)
sFmap1 sg sx = sTo1 (sFmap sg (sFrom1 sx))

sFmap2  :: forall (p :: Type) (a :: Type) (b :: Type) (g :: b) (x :: (p, a)).
          Proxy g -> Proxy x -> Proxy (Fmap g x)
sFmap2 = sFmap1
