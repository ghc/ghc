{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T16646Fail3 where

import Data.Kind
import Data.Proxy
import GHC.Exts

class Reifies s a | s -> a where
  reflect :: proxy s -> a

reify :: forall a r. a -> (forall (s :: Type). Reifies s a => Proxy s -> r) -> r
reify a k = magicDict @(Reifies (Any @Type) a) (k @Any) (const a) Proxy
