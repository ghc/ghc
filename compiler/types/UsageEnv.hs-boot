module UsageEnv where

import GhcPrelude
import Name
import Outputable
import Weight
import {-# SOURCE #-} TyCoRep (Rig)

data UsageEnv
instance Outputable UsageEnv

unitUE :: NamedThing n => n -> Rig -> UsageEnv
zeroUE, emptyUE :: UsageEnv
mapUE :: (Rig -> Rig) -> UsageEnv -> UsageEnv
allUE :: (Rig -> Bool) -> UsageEnv -> Bool

data IsSubweight = Smaller | Larger | Unknown

subweightMaybe :: GMult t -> GMult t -> IsSubweight
