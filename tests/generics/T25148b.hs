{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
module T25148b where

import Data.Functor.Const
import Data.Proxy

class Monoid a => C a where
  m :: forall {k} (b :: k). Const a b
  m = Const mempty

class Monoid a => C2 a where
  m2 :: forall {k} {b :: k}. Const a b
  m2 = Const mempty

class Monoid a => C3 a where
  m3 :: forall {k} (b :: k) {p :: Proxy b}. Const a p
  m3 = Const mempty

data VisProxy k (a :: k) = VisProxy

class Monoid a => C4 a where
  m4 :: forall {k} (b :: k) {p :: VisProxy k b}. Const a p
  m4 = Const mempty

type family Any :: k

class Monoid a => C5 a where
  m5 :: Proxy Any -> a
  m5 _ = mempty

-----

data T = MkT
  deriving anyclass (C, C2, C3, C4, C5)

instance Semigroup T where
  _ <> _ = MkT
instance Monoid T where
  mempty = MkT
