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

class Reifies s a | s -> a where
  reflect :: proxy s -> a

reify :: forall a r. a -> (forall (s :: Type). Reifies s a => Proxy s -> r) -> r
reify a k = withDict @(Reifies (Any @Type) a) @_ (const a) (k @Any) Proxy
