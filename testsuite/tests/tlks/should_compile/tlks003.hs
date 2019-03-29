{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies, PolyKinds, ExplicitForAll #-}

module TLKS_003 where

type InjectiveId :: forall k. k -> k
type family InjectiveId x = r | r -> x where
  InjectiveId x = x
