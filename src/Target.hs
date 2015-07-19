{-# LANGUAGE DeriveGeneric, TypeSynonymInstances #-}
module Target (
    Target (..), StageTarget (..), StagePackageTarget (..), FullTarget (..),
    stageTarget, stagePackageTarget, fullTarget, fullTargetWithWay
    ) where

import Way
import Stage
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
        getFiles   :: [FilePath],
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
        getFiles   = error "stageTarget: Files not set",
        getBuilder = error "stageTarget: Builder not set",
        getWay     = vanilla -- most targets are built only one way (vanilla)
    }

-- StagePackageTarget is a Target whose fields getStage and getPackage are
-- already assigned
type StagePackageTarget = Target

stagePackageTarget :: Stage -> Package -> StagePackageTarget
stagePackageTarget stage package = Target
    {
        getStage   = stage,
        getPackage = package,
        getFiles   = error "stagePackageTarget: Files not set",
        getBuilder = error "stagePackageTarget: Builder not set",
        getWay     = vanilla
    }

-- FullTarget is a Target whose fields are all assigned
type FullTarget = Target

-- Most targets are built only one way, vanilla, hence we set it by default.
fullTarget :: StagePackageTarget -> [FilePath] -> Builder -> FullTarget
fullTarget target files builder = target
    {
        getFiles   = files,
        getBuilder = builder,
        getWay     = vanilla
    }

-- Use this function to be explicit about build the way.
fullTargetWithWay :: StagePackageTarget -> [FilePath] -> Builder -> Way -> FullTarget
fullTargetWithWay target files builder way = target
    {
        getFiles   = files,
        getBuilder = builder,
        getWay     = way
    }

-- Shows a (full) target as "package:file@stage (builder, way)"
instance Show FullTarget where
    show target = show (getPackage target)
                  ++ ":" ++ show (getFiles target)
                  ++ "@" ++ show (getStage target)
                  ++ " (" ++ show (getBuilder target)
                  ++ ", " ++ show (getWay target) ++ ")"

-- Instances for storing in the Shake database
instance Binary FullTarget
instance NFData FullTarget
instance Hashable FullTarget
