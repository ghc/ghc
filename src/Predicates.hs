-- | Convenient predicates
module Predicates (
    stage, package, builder, stagedBuilder, builderGcc, builderGhc, file, way,
    stage0, stage1, stage2, notStage0, notPackage, registerPackage
    ) where

import Base
import Expression

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

builderGcc :: Predicate
builderGcc = stagedBuilder Gcc ||^ stagedBuilder GccM

builderGhc :: Predicate
builderGhc = stagedBuilder Ghc ||^ stagedBuilder GhcM

file :: FilePattern -> Predicate
file f = fmap (any (f ?==)) getOutputs

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

notPackage :: Package -> Predicate
notPackage = notM . package

-- TODO: Actually, we don't register compiler in some circumstances -- fix.
registerPackage :: Predicate
registerPackage = return True
