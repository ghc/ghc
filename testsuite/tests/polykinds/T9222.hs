{-# LANGUAGE RankNTypes, GADTs, DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}
module T9222 where

import Data.Proxy

data Want :: (i,j) -> * where
  Want :: (a ~ '(b,c) => Proxy b) -> Want a
