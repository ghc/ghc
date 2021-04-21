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

f :: forall {rr :: RuntimeRep} dt st (r :: TYPE rr). (dt => r) -> st -> r
f = magicDict @dt

class Show a => C a where
  m :: Maybe a

g :: forall a r. (C a => r) -> Maybe a -> r
g = magicDict @(C a)

class Reifies s a | s -> a where
  reflect :: proxy s -> a

reify :: forall a r. a -> (forall (s :: Type). Reifies s a => Proxy s -> r) -> r
reify a k = magicDict @(Reifies (Any @Type) a) (k @Any) (const a) Proxy
