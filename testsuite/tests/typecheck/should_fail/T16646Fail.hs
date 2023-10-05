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

f :: forall {rr :: RuntimeRep} cls meth (r :: TYPE rr). meth -> (cls => r) -> r
f = withDict @cls @meth

class Show a => C a where
  m :: Maybe a

g :: forall a r. Maybe a -> (C a => r) -> r
g = withDict @(C a) @(Maybe a)
