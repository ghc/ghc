{-# LANGUAGE ScopedTypeVariables, RankNTypes, PolyKinds #-}
module T8856 where

import Data.Proxy

foo = (undefined :: Proxy a) :: forall a. Proxy a
