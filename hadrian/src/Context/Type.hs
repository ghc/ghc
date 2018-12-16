module Context.Type where

import Development.Shake.Classes
import GHC.Generics
import Hadrian.Package

import Stage
import Way.Type

-- | Build context for a currently built 'Target'. We generate potentially
-- different build rules for each 'Context'.
data Context = Context
    { stage   :: Stage   -- ^ Currently build Stage
    , package :: Package -- ^ Currently build Package
    , way     :: Way     -- ^ Currently build Way (usually 'vanilla')
    } deriving (Eq, Generic, Show)

instance Binary   Context
instance Hashable Context
instance NFData   Context

-- | Which libraries we need to use when building a certain stage.
stageLibraries :: Stage -> Stage
stageLibraries Stage0 = Stage0
stageLibraries Stage1 = Stage1
stageLibraries Stage2 = Stage1
stageLibraries Stage3 = Stage2
