{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RequiredTypeArguments #-}

module RetainedDot where

data Flag (b :: Bool) where
    On :: Flag True
    Off :: Flag False

data RetainedFlag where
    RuntimeFlag :: foreach (b :: Bool) -> Flag b -> RetainedFlag

type Hidden = exists (b :: Bool). Flag b

forget :: RetainedFlag -> Hidden
forget (RuntimeFlag _ payload) = payload

retained :: Bool -> RetainedFlag
retained True = RuntimeFlag True On
retained False = RuntimeFlag False Off

observe :: Hidden -> Bool
observe p = flagValue p
  where
    flagValue :: Flag b -> Bool
    flagValue On = True
    flagValue Off = False
