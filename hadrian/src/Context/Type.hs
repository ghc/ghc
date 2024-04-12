module Context.Type where

import Development.Shake.Classes
import GHC.Generics
import Hadrian.Package

import Stage
import Way.Type
import Way

-- | Build context for a currently built 'Target'. We generate potentially
-- different build rules for each 'Context'.
data Context = Context
    { stage   :: Stage   -- ^ Currently build Stage
    , package :: Package -- ^ Currently build Package
    , way     :: Way     -- ^ Currently build Way (usually 'vanilla')
    , iplace  :: Inplace -- ^ Whether to use the inplace or final package database
    } deriving (Eq, Generic, Show)

instance Binary   Context
instance Hashable Context
instance NFData   Context

-- | Most targets are built only one way, hence the notion of 'vanillaContext'.
vanillaContext :: Stage -> Package -> Context
vanillaContext s p = Context s p vanilla Final

-- | Partial context with undefined 'Package' field. Useful for 'Packages'
-- expressions that only read the environment and current 'Stage'.
stageContext :: Stage -> Context
stageContext s = vanillaContext s $ error "stageContext: package not set"
