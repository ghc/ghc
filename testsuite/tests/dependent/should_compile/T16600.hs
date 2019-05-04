{-# LANGUAGE RankNTypes, PolyKinds #-}

module T16600 where

import Data.Kind

data Q :: forall d. Type

data HR :: (forall d. Type) -> Type

x :: HR Q
x = undefined
