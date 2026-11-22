{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RequiredTypeArguments #-}

module RetainedDot where

data Flag (b :: Bool) where
    On :: Flag True
    Off :: Flag False

data RetainedFlag where
    RuntimeFlag :: foreach (b :: Bool) -> Flag b -> RetainedFlag

type Hidden = exists (b :: Bool). Flag b

data Value a (b :: Bool) = Value a

data Choice a (b :: Bool) where
    Choice :: foreach (b :: Bool) -> Value a b -> Choice a b

type HiddenChoice a = exists (b :: Bool). Choice a b

type IgnoreFlag (b :: Bool) = Char
type HiddenConstant = exists (b :: Bool). Choice (Maybe (IgnoreFlag b)) b

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
