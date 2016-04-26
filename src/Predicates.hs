{-# LANGUAGE LambdaCase, FlexibleInstances #-}
-- | Convenient predicates
module Predicates (
    stage, package, builder, file, way, stage0, stage1, stage2, notStage0, notPackage
    ) where

import Base
import Expression

-- | Is the build currently in the provided stage?
stage :: Stage -> Predicate
stage s = (s ==) <$> getStage

-- | Is a particular package being built?
package :: Package -> Predicate
package p = (p ==) <$> getPackage

-- TODO: Also add needBuilder, builderPath, etc.
-- | Is a particular builder being used?
class BuilderLike a where
    builder :: a -> Predicate

instance BuilderLike Builder where
    builder b = (b ==) <$> getBuilder

instance BuilderLike a => BuilderLike (Stage -> a) where
    builder stagedBuilder = builder . stagedBuilder =<< getStage

instance BuilderLike a => BuilderLike (CompilerMode -> a) where
    builder compiler = anyM (builder . compiler) [Compile, FindDependencies, Link]

-- | Does any of the output files match a given pattern?
file :: FilePattern -> Predicate
file f = any (f ?==) <$> getOutputs

-- | Is the current build 'Way' equal to a certain value?
way :: Way -> Predicate
way w = (w ==) <$> getWay

-- | Is the build currently in stage 0?
stage0 :: Predicate
stage0 = stage Stage0

-- | Is the build currently in stage 1?
stage1 :: Predicate
stage1 = stage Stage1

-- | Is the build currently in stage 2?
stage2 :: Predicate
stage2 = stage Stage2

-- | Is the build /not/ in stage 0 right now?
notStage0 :: Predicate
notStage0 = notM stage0

-- | Is a certain package /not/ built right now?
notPackage :: Package -> Predicate
notPackage = notM . package
