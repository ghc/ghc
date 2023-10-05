{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, PolyKinds, ExplicitForAll #-}

module SAKS_002 where

type Id :: forall k. k -> k
type family Id x where
  Id x = x
