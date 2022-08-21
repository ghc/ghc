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
    , iplace  :: Inplace -- ^ Whether to use the inplace or final package database
    } deriving (Eq, Generic, Show)

instance Binary   Context
instance Hashable Context
instance NFData   Context
