{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Target (
    Target (..), StageTarget, StagePackageTarget, FullTarget,
    stageTarget, stagePackageTarget, fullTarget, fullTargetWithWay,
    ) where

import Way
import Base
import Stage
import Package
import Builder
import GHC.Generics
import Data.Monoid
import Control.Monad.Reader

-- Target captures all parameters relevant to the current build target:
-- * Stage and Package being built,
-- * Builder to be invoked,
-- * Way to be built (set to vanilla for most targets),
-- * source file(s) to be passed to Builder,
-- * file(s) to be produced.
data Target = Target
     {
        stage   :: Stage,
        package :: Package,
        builder :: Builder,
        way     :: Way,
        sources :: [FilePath],
        files   :: [FilePath]
     }
     deriving (Show, Eq, Generic)

-- If values of type 'a' form a Monoid then we can also derive a Monoid instance
-- for values of type 'ReaderT Target Action a':
-- * the empty computation returns the identity element of the underlying type
-- * two computations can be combined by combining their results
instance Monoid a => Monoid (ReaderT Target Action a) where
    mempty  = return mempty
    mappend = liftM2 mappend

-- StageTarget is a partially constructed Target. Only stage is guaranteed to
-- be assigned.
type StageTarget = Target

stageTarget :: Stage -> StageTarget
stageTarget s = Target
    {
        stage   = s,
        package = error "stageTarget: package not set",
        builder = error "stageTarget: builder not set",
        way     = vanilla,
        sources = error "stageTarget: sources not set",
        files   = error "stageTarget: files not set"
    }

-- StagePackageTarget is a partially constructed Target. Only stage and package
-- are guaranteed to be assigned.
type StagePackageTarget = Target

stagePackageTarget :: Stage -> Package -> StagePackageTarget
stagePackageTarget s p = Target
    {
        stage   = s,
        package = p,
        builder = error "stagePackageTarget: builder not set",
        way     = vanilla,
        sources = error "stagePackageTarget: sources not set",
        files   = error "stagePackageTarget: files not set"
    }

-- FullTarget is a Target whose fields are all assigned
type FullTarget = Target

-- Most targets are built only one way, vanilla, hence we set it by default.
fullTarget :: StagePackageTarget -> Builder -> [FilePath] -> [FilePath] -> FullTarget
fullTarget target b srcs fs = target
    {
        builder = b,
        way     = vanilla,
        sources = srcs,
        files   = fs
    }

-- Use this function to be explicit about the build way.
fullTargetWithWay :: StagePackageTarget -> Builder -> Way -> [FilePath] -> [FilePath] -> FullTarget
fullTargetWithWay target b w srcs fs = target
    {
        builder = b,
        way     = w,
        sources = srcs,
        files   = fs
    }

-- Instances for storing in the Shake database
instance Binary FullTarget
instance NFData FullTarget
instance Hashable FullTarget
