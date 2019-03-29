{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE TypeFamilies, PolyKinds, ExplicitForAll #-}

module TLKS_002 where

type Id :: forall k. k -> k
type family Id x where
  Id x = x
