{-# LANGUAGE GADTs, PolyKinds, RankNTypes #-}

module GADTVars where

import Data.Kind
import Data.Proxy

data T (k1 :: Type) (k2 :: Type) (a :: k2) (b :: k2) where
  MkT :: T x1 Type (Proxy (y :: x1), z) z
