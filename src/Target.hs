{-# LANGUAGE DeriveGeneric, TypeSynonymInstances #-}
module Target (
    Target (..), StageTarget (..), StagePackageTarget (..), FullTarget (..),
    stageTarget, stagePackageTarget, fullTarget
    ) where

import Way
import Base
import Package
import Builder
import GHC.Generics
import Development.Shake.Classes

-- Target captures parameters relevant to the current build target: Stage and
-- Package being built, Builder that is to be invoked, file(s) that are to
-- be built and the Way they are to be built.
data Target = Target
     {
        getStage   :: Stage,
        getPackage :: Package,
        getFile    :: FilePath, -- TODO: handle multple files?
        getBuilder :: Builder,
        getWay     :: Way
     }
     deriving (Eq, Generic)

-- StageTarget is a Target whose field getStage is already assigned
type StageTarget = Target

stageTarget :: Stage -> StageTarget
stageTarget stage = Target
    {
        getStage   = stage,
        getPackage = error "stageTarget: Package not set",
        getFile    = error "stageTarget: File not set",
        getBuilder = error "stageTarget: Builder not set",
        getWay     = error "stageTarget: Way not set"
    }

-- StagePackageTarget is a Target whose fields getStage and getPackage are
-- already assigned
type StagePackageTarget = Target

stagePackageTarget :: Stage -> Package -> StagePackageTarget
stagePackageTarget stage package = Target
    {
        getStage   = stage,
        getPackage = package,
        getFile    = error "stagePackageTarget: File not set",
        getBuilder = error "stagePackageTarget: Builder not set",
        getWay     = error "stagePackageTarget: Way not set"
    }

-- FullTarget is a Target whose fields are all assigned
type FullTarget = Target

fullTarget :: StagePackageTarget -> FilePath -> Builder -> Way -> FullTarget
fullTarget target file builder way = target
    {
        getFile    = file,
        getBuilder = builder,
        getWay     = way
    }

-- Shows a (full) target as "package:file@stage (builder, way)"
instance Show FullTarget where
    show target = show (getPackage target)
                  ++ ":" ++ getFile target
                  ++ "@" ++ show (getStage target)
                  ++ " (" ++ show (getBuilder target)
                  ++ ", " ++ show (getWay target) ++ ")"

-- Instances for storing in the Shake database
instance Binary FullTarget
instance NFData FullTarget
instance Hashable FullTarget
