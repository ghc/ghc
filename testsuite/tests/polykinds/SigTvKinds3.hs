{-# LANGUAGE GADTs, ExplicitForAll, TypeInType #-}

module SigTvKinds3 where

import Data.Kind

data SameKind :: k -> k -> Type
data Bad a where
  MkBad :: forall k1 k2 (a :: k1) (b :: k2). Bad (SameKind a b)
