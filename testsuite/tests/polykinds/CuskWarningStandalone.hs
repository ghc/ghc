{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExplicitForAll, GADTs, PolyKinds, StandaloneKindSignatures, CUSKs #-}
module CuskWarningStandalone where

import Data.Kind (Type)

type Visible :: forall k -> k -> Type
data Visible k a where
  MkVisible :: Visible (k1 -> k2) f -> Visible k1 a -> Visible k2 (f a)
