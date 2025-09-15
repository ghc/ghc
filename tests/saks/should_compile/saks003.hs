{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies, PolyKinds, ExplicitForAll #-}

module SAKS_003 where

type InjectiveId :: forall k. k -> k
type family InjectiveId x = r | r -> x where
  InjectiveId x = x
