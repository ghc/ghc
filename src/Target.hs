{-# LANGUAGE DeriveGeneric #-}
module Target (
    Target (..), stageTarget, stagePackageTarget
    ) where

import Base
import Ways
import Package
import Oracles.Builder
import GHC.Generics
import Development.Shake.Classes

-- Target captures parameters relevant to the current build target: Stage and
-- Package being built, Builder that is to be invoked, file(s) that are to
-- be built and the Way they are to be built.
data Target = Target
     {
        getStage   :: Stage,
        getPackage :: Package,
        getBuilder :: Builder,
        getFile    :: FilePath, -- TODO: handle multple files?
        getWay     :: Way
     }
     deriving (Eq, Generic)

-- Shows a target as "package:file@stage (builder, way)"
instance Show Target where
    show target = show (getPackage target)
                  ++ ":" ++ show (getFile target)
                  ++ "@" ++ show (getStage target)
                  ++ " (" ++ show (getBuilder target)
                  ++ ", " ++ show (getWay target) ++ ")"

stageTarget :: Stage -> Target
stageTarget stage = Target
    {
        getStage   = stage,
        getPackage = error "stageTarget: Package not set",
        getBuilder = error "stageTarget: Builder not set",
        getFile    = error "stageTarget: File not set",
        getWay     = error "stageTarget: Way not set"
    }

stagePackageTarget :: Stage -> Package -> Target
stagePackageTarget stage package = Target
    {
        getStage   = stage,
        getPackage = package,
        getBuilder = error "stagePackageTarget: Builder not set",
        getFile    = error "stagePackageTarget: File not set",
        getWay     = error "stagePackageTarget: Way not set"
    }

-- Instances for storing Target in the Shake database
instance Binary Target
instance NFData Target
instance Hashable Target
