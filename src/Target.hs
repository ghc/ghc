{-# LANGUAGE DeriveGeneric, TypeSynonymInstances #-}
module Target (
    Target (..), StageTarget (..), StagePackageTarget (..), FullTarget (..),
    stageTarget, stagePackageTarget, fullTarget, fullTarwithWay
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
        stage   :: Stage,
        package :: Package,
        files   :: [FilePath],
        builder :: Builder,
        way     :: Way
     }
     deriving (Eq, Generic)

-- StageTarget is a partially constructed Target. Only stage is guaranteed to
-- be assigned.
type StageTarget = Target

stageTarget :: Stage -> StageTarget
stageTarget s = Target
    {
        stage   = s,
        package = error "stageTarget: Package not set",
        files   = error "stageTarget: Files not set",
        builder = error "stageTarget: Builder not set",
        way     = vanilla
    }

-- StagePackageTarget is a partially constructed Target. Only stage and package
-- are guaranteed to be assigned.
type StagePackageTarget = Target

stagePackageTarget :: Stage -> Package -> StagePackageTarget
stagePackageTarget s p = Target
    {
        stage   = s,
        package = p,
        files   = error "stagePackageTarget: Files not set",
        builder = error "stagePackageTarget: Builder not set",
        way     = vanilla
    }

-- FullTarget is a Target whose fields are all assigned
type FullTarget = Target

-- Most targets are built only one way, vanilla, hence we set it by default.
fullTarget :: StagePackageTarget -> [FilePath] -> Builder -> FullTarget
fullTarget target fs b = target
    {
        files   = fs,
        builder = b,
        way     = vanilla
    }

-- Use this function to be explicit about the build way.
fullTarwithWay :: StagePackageTarget -> [FilePath] -> Builder -> Way -> FullTarget
fullTarwithWay target fs b w = target
    {
        files   = fs,
        builder = b,
        way     = w
    }

-- Shows a (full) target as "package:file@stage (builder, way)"
instance Show FullTarget where
    show target = show (package target)
                  ++ ":" ++ show (files target)
                  ++ "@" ++ show (stage target)
                  ++ " (" ++ show (builder target)
                  ++ ", " ++ show (way target) ++ ")"

-- Instances for storing in the Shake database
instance Binary FullTarget
instance NFData FullTarget
instance Hashable FullTarget
