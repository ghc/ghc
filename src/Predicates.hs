-- | Convenient predicates
module Predicates (
    stage, package, builder, stagedBuilder, builderGcc, builderGhc, file, way,
    stage0, stage1, stage2, notStage0, notPackage, registerPackage
    ) where

import Base
import Expression

-- | Is the build currently in the provided stage?
stage :: Stage -> Predicate
stage s = (s ==) <$> getStage

-- | Is a particular package being built?
package :: Package -> Predicate
package p = (p ==) <$> getPackage

-- | Is an unstaged builder is being used such as /GhcCabal/?
builder :: Builder -> Predicate
builder b = (b ==) <$> getBuilder

-- | Is a certain builder used in the current stage?
stagedBuilder :: (Stage -> Builder) -> Predicate
stagedBuilder stageBuilder = do
    s <- getStage
    builder (stageBuilder s)

-- | Are we building with GCC?
builderGcc :: Predicate
builderGcc = stagedBuilder Gcc ||^ stagedBuilder GccM

-- | Are we building with GHC?
builderGhc :: Predicate
builderGhc = stagedBuilder Ghc ||^ stagedBuilder GhcM

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

-- TODO: Actually, we don't register compiler in some circumstances -- fix.
-- | Do we need to run @ghc-pkg update@ on the currently built package?
-- See "Rules.Data".
registerPackage :: Predicate
registerPackage = return True
