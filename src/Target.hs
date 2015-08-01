{-# LANGUAGE DeriveGeneric, TypeSynonymInstances #-}
module Target (
    Target (..), StageTarget (..), StagePackageTarget (..), FullTarget (..),
    stageTarget, stagePackageTarget, fullTarget, fullTargetWithWay,
    ) where

import Way
import Base
import Stage
import Package
import Builder
import GHC.Generics

-- Target captures all parameters relevant to the current build target:
-- * Stage and Package being built,
-- * dependencies (e.g., source files) that need to be tracked,
-- * Builder to be invoked,
-- * Way to be built (set to vanilla for most targets),
-- * file(s) to be produced.
data Target = Target
     {
        stage        :: Stage,
        package      :: Package,
        dependencies :: [FilePath],
        builder      :: Builder,
        way          :: Way,
        files        :: [FilePath]
     }
     deriving (Show, Eq, Generic)

-- StageTarget is a partially constructed Target. Only stage is guaranteed to
-- be assigned.
type StageTarget = Target

stageTarget :: Stage -> StageTarget
stageTarget s = Target
    {
        stage        = s,
        package      = error "stageTarget: package not set",
        dependencies = error "stageTarget: dependencies not set",
        builder      = error "stageTarget: builder not set",
        way          = vanilla,
        files        = error "stageTarget: files not set"
    }

-- StagePackageTarget is a partially constructed Target. Only stage and package
-- are guaranteed to be assigned.
type StagePackageTarget = Target

stagePackageTarget :: Stage -> Package -> StagePackageTarget
stagePackageTarget s p = Target
    {
        stage        = s,
        package      = p,
        dependencies = error "stagePackageTarget: dependencies not set",
        builder      = error "stagePackageTarget: builder not set",
        way          = vanilla,
        files        = error "stagePackageTarget: files not set"
    }

-- FullTarget is a Target whose fields are all assigned
type FullTarget = Target

-- Most targets are built only one way, vanilla, hence we set it by default.
fullTarget :: StagePackageTarget -> [FilePath] -> Builder -> [FilePath] -> FullTarget
fullTarget target deps b fs = target
    {
        dependencies = deps,
        builder      = b,
        way          = vanilla,
        files        = fs
    }

-- Use this function to be explicit about the build way.
fullTargetWithWay :: StagePackageTarget -> [FilePath] -> Builder -> Way
                  -> [FilePath] -> FullTarget
fullTargetWithWay target deps b w fs = target
    {
        dependencies = deps,
        builder      = b,
        way          = w,
        files        = fs
    }

-- Instances for storing in the Shake database
instance Binary FullTarget
instance NFData FullTarget
instance Hashable FullTarget
