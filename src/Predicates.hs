module Predicates (
    module GHC,
    module Oracles.Config.Flag,
    module Oracles.Config.Setting,
    stage, package, builder, stagedBuilder, file, way,
    stage0, stage1, stage2, notStage0, registerPackage, splitObjects
    ) where

import Expression
import GHC
import Oracles.Config.Flag
import Oracles.Config.Setting

-- Basic predicates
stage :: Stage -> Predicate
stage s = fmap (s ==) getStage

package :: Package -> Predicate
package p = fmap (p ==) getPackage

-- For unstaged builders, e.g. GhcCabal
builder :: Builder -> Predicate
builder b = fmap (b ==) getBuilder

-- For staged builders, e.g. Ghc Stage
stagedBuilder :: (Stage -> Builder) -> Predicate
stagedBuilder sb = (builder . sb) =<< getStage

file :: FilePattern -> Predicate
file f = fmap (any (f ?==)) getFiles

way :: Way -> Predicate
way w = fmap (w ==) getWay

-- Derived predicates
stage0 :: Predicate
stage0 = stage Stage0

stage1 :: Predicate
stage1 = stage Stage1

stage2 :: Predicate
stage2 = stage Stage2

notStage0 :: Predicate
notStage0 = notM stage0

-- TODO: Actually, we don't register compiler in some circumstances -- fix.
registerPackage :: Predicate
registerPackage = return True

splitObjects :: Predicate
splitObjects = do
    goodStage   <- notStage0 -- We don't split bootstrap (stage 0) packages
    goodPackage <- notM $ package compiler -- We don't split compiler
    supported   <- lift supportsSplitObjects
    return $ goodStage && goodPackage && supported
