{-# LANGUAGE ScopedTypeVariables #-}
module T16646Fail2 where

import GHC.Exts

class Show a => C a where
  m :: Maybe a

f :: forall a r. (C a => r) -> Maybe a -> r
f = magicDict @(C a)
