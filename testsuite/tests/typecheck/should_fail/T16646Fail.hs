{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module T16646Fail where

import Data.Kind
import Data.Proxy
import GHC.Exts

f :: forall {rr :: RuntimeRep} st dt (r :: TYPE rr). st -> (dt => r) -> r
f = withDict @st @dt

class Show a => C a where
  m :: Maybe a

g :: forall a r. Maybe a -> (C a => r) -> r
g = withDict @(Maybe a) @(C a)

class Reifies s a | s -> a where
  reflect :: proxy s -> a

reify :: forall a r. a -> (forall (s :: Type). Reifies s a => Proxy s -> r) -> r
reify a k = withDict @_ @(Reifies (Any @Type) a) (const a) (k @Any) Proxy
