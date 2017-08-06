{-# LANGUAGE DeriveGeneric #-}
module Context (
    Context (..), vanillaContext, stageContext, getStage, getPackage, getWay
    ) where

import GHC.Generics
import Hadrian.Expression

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

instance Binary   Context
instance Hashable Context
instance NFData   Context

-- | Most targets are built only one way, hence the notion of 'vanillaContext'.
vanillaContext :: Stage -> Package -> Context
vanillaContext s p = Context s p vanilla

-- | Partial context with undefined 'Package' field. Useful for 'Packages'
-- expressions that only read the environment and current 'Stage'.
stageContext :: Stage -> Context
stageContext s = vanillaContext s $ error "stageContext: package not set"

-- | Get the 'Stage' of the current 'Context'.
getStage :: Expr Context b Stage
getStage = stage <$> getContext

-- | Get the 'Package' of the current 'Context'.
getPackage :: Expr Context b Package
getPackage = package <$> getContext

-- | Get the 'Way' of the current 'Context'.
getWay :: Expr Context b Way
getWay = way <$> getContext

