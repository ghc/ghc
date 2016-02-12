{-# LANGUAGE DeriveGeneric #-}
module Context (Context (..), vanillaContext, stageContext) where

import GHC.Generics (Generic)

import Base
import Package
import Stage
import Way

-- | Build context for a currently built 'Target'. We generate potentially
-- different build rules for each 'Context'.
data Context = Context
    { stage   :: Stage   -- ^ Currently build Stage
    , package :: Package -- ^ Currently build Package
    , way     :: Way     -- ^ Currently build Way (usually 'vanilla')
    } deriving (Show, Eq, Generic)

-- | Most targets are built only one way, hence the notion of 'vanillaContext'.
vanillaContext :: Stage -> Package -> Context
vanillaContext s p = Context s p vanilla

stageContext :: Stage -> Context
stageContext s = vanillaContext s $ error "stageContext: package not set"

instance Binary Context
instance NFData Context
instance Hashable Context
