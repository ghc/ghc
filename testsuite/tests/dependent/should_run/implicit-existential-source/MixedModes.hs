{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module MixedModes where

import Data.Proxy (Proxy)

type Stored = some (b :: Bool) | Proxy b
type Hidden = exists (b :: Bool). Proxy b
